/**
* Arpha programming language compiler.
* (c) 2012
*/

#include "base/base.h"
#include "base/symbol.h"
#include "base/system.h"
#include "compiler.h"
#include "ast/scope.h"
#include "ast/node.h"
#include "ast/declarations.h"
#include "ast/resolve.h"
#include "ast/interpret.h"
#include "syntax/parser.h"
#include "intrinsics/types.h"

#include "data/data.h"

#include "gen/llvm/gen.h"
#include "gen/linker.h"

namespace arpha {
	void defineCoreSyntax(Scope* scope);
	void defineIntrinsicSyntax(Scope* scope);

	BlockExpression* parseModule(Parser* parser,BlockExpression* block);
};

void runTests();


namespace compiler {

	struct Module {
		std::string directory;
		Scope* scope;
		Node*  body;
		bool compile;
		size_t errorCount;
		const char* src;
	};

	typedef std::map<std::string,Module>::iterator ModulePtr;
	std::map<std::string,Module> modules;
	ModulePtr currentModule;

	std::string packageDir;

	std::map<std::string,void (*)(Scope*)> postCallbacks;

	ModulePtr findByScope(Scope* scope){
		if(scope == currentModule->second.scope) return currentModule;
		for(auto i = modules.begin();i!=modules.end();++i){
			if(i->second.scope == scope) return i;
		}
		return currentModule;
	}


	void registerResolvedIntrinsicModuleCallback(const char* name,void (* f)(Scope*)){
		postCallbacks[packageDir+"/"+name+".arp"] = f;
	}

	Interpreter* interpreter;

	CompilationUnit _currentUnit;

	CompilationUnit *currentUnit(){
		return &_currentUnit;
	}

	void addGeneratedExpression(Node* expr){
		if(!generatedFunctions) generatedFunctions = new BlockExpression;
		generatedFunctions->addChild(expr);
	}

	BlockExpression* generatedFunctions = nullptr;


	ModulePtr newModule(const char* moduleName,const char* source){
		Module module = {};
		auto insertionResult = modules.insert(std::make_pair(std::string(moduleName),module));

		auto prevModule = currentModule;
		currentModule = insertionResult.first;

		currentModule->second.directory = System::path::directory(moduleName);
		currentModule->second.errorCount = 0;
		currentModule->second.src = source;

		//module
		auto block = new BlockExpression();
		//Special case for 'packages/arpha/arp.arp'
		if((packageDir + "/arpha/arpha.arp") == moduleName){
			//import 'arpha' by default
			arpha::defineCoreSyntax(block->scope);
			compiler::registerResolvedIntrinsicModuleCallback("arpha/types",intrinsics::types::preinit);
			compiler::registerResolvedIntrinsicModuleCallback("arpha/intrinsic",arpha::defineIntrinsicSyntax);
		}
		else {
			auto cb = postCallbacks.find(moduleName);
			if(cb != postCallbacks.end()) (*cb).second(block->scope);
			//import 'arpha' by default
			block->scope->import(findModule("arpha"),"arpha");
		}
		currentModule->second.scope = block->scope;
		currentModule->second.body = block;

		
		
		auto prevUnit = _currentUnit;
		
		Parser   parser(source,&_currentUnit);
		Resolver resolver(&_currentUnit);
		_currentUnit.resolver    = &resolver;
		_currentUnit.interpreter = interpreter;
		_currentUnit.parser      = &parser;
		_currentUnit.moduleBody  = block;
		_currentUnit.printingDecorationLevel = 1;
		
		arpha::parseModule(&parser,block);

		debug("------------------- AST: ------------------------------");
		debug("%s\n",block);

		resolver.resolveModule(block);

		currentModule->second.src = nullptr;
		//restore old module ptr
		currentModule = prevModule;
		_currentUnit = prevUnit;
		return insertionResult.first;
	}

	ModulePtr newModuleFromFile(const char* filename){
		auto src = System::fileToString(filename);
		auto module = newModule(filename,(const char*)src);
		System::free((void*)src);
		return module;
	}

	//Module importing is done by searching in the appropriate directories
	Scope* findModuleFromDirectory(std::string& dir,const char* name){
		//Try non package way
		auto filename = dir + "/" + name + ".arp";
		if(!System::fileExists(filename.c_str())){
			//Try package way
			filename = dir + "/" + name + "/" + System::path::filename(name) + ".arp";
			if(!System::fileExists(filename.c_str())) return nullptr;
		}
		//load module
		auto module = modules.find(filename);
		if(module == modules.end()){
			onDebug(format("A new module %s located at '%s' will be loaded.",name,filename));
			module = newModuleFromFile(filename.c_str());
		}
		return module->second.scope;
	}
	//Finds a module and loads it if necessary to match the existing name
	Scope* findModule(const char* name){
		//Search in the current directory
		if(currentModule != modules.end()){
			auto module = findModuleFromDirectory(currentModule->second.directory,name);
			if(module) return module;
		}
		//Search in the packages directory for a package
		return findModuleFromDirectory(packageDir,name);
	}

	int reportLevel;

	void init(data::Options* options){
		interpreter = constructInterpreter(nullptr);
		currentModule = modules.end();
		packageDir = options->packagesPaths[0];//TODO use all paths

		reportLevel = ReportErrors;

		intrinsics::types::startup();

		//Load language definitions.
		newModuleFromFile((packageDir + "/arpha/arpha.arp").c_str());

		reportLevel = ReportDebug;
	}

	void onDebug(const std::string& message){
		if(reportLevel >= ReportDebug) System::debugPrint(message);
	}
	void showSourceLine(Location& location,size_t offset = 0){
		size_t i;
		for(i = 0;i<offset;i++) std::cout<<' ';
		const char* src = currentModule->second.src;
		for(i = 0;i<location.line();src++){
			if(*src == '\0') break;
			else if(*src == '\n') i++;
		}
		for(;*src!='\n' && *src!='\r' && *src!='\0';src++) std::cout<<*src;
		if(location.column >= 0){
			std::cout<< std::endl;
			for(i = 0;i<offset;i++) std::cout<<' ';
			for(i = 0;i<location.column;i++) std::cout<<'~';
			std::cout<<"^";
		}
		std::cout<< std::endl;
	}
	void onError(Location& location,const std::string& message){
		if(reportLevel >= ReportErrors){
			std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": Error: " << message << std::endl;
			showSourceLine(location,currentModule->first.size());
		}
		currentModule->second.errorCount++;
	}
	void onError(Node* node,const std::string& message){
		auto location = node->location();
		if(reportLevel >= ReportErrors)
			std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": Error: " << message << std::endl;
		currentModule->second.errorCount++;
	}
	void intrinsicFatalError(Location& location,const std::string& message){
		std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": INTRINSIC FATAL ERROR: " << message << std::endl;
		std::cout<<"The compiler will now exit!"<<std::endl;
		currentModule->second.errorCount++;
	}
	void headError(Location& location,const std::string& message){
		std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": Error: " << message << std::endl;
		currentModule->second.errorCount++;
	}
	void subError(Location& location,const std::string& message){
		for(size_t i = 0;i<currentModule->first.size();i++) std::cout<<' ';
		std::cout<< '(' << location.line() << ':' << location.column << ')' <<": " << message << std::endl;
		showSourceLine(location,currentModule->first.size());
		currentModule->second.errorCount++;
	}
	void onAmbiguosDeclarationError(Node* declaration){
		
		if(auto function = declaration->asFunction()){
			auto module = findByScope(function->owner()->moduleScope());
			System::print(format("\t%s(%d:%d): def %s(",module->first,function->location().lineNumber,function->location().column,function->label()));
			for(auto i = function->arguments.begin();i!=function->arguments.end();++i)
				System::print(format("%s %s%c ",(*i)->label(),(*i)->type,(i+1)==function->arguments.end()? ')':','));
			System::print("\n");
		} else assert(false);
	}
	void onWarning(Location& location,const std::string& message){
		std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": Warning: " << message << std::endl;
		showSourceLine(location,currentModule->first.size());
	}



}

void onError(const std::string& message){
	System::print("Error: ");
	System::print(message);
	System::print("\n");
}
void onWarning(const std::string& message){
	System::print("Warning: ");
	System::print(message);
	System::print("\n");
}
void onFatalError(const std::string& message){
	System::print("Fatal error: ");
	System::print(message);
	System::print("\n");
	exit(-1);
}

void createDefaultTarget(data::gen::native::Target& target){
#ifdef _WIN32
	target.platform = data::gen::native::Target::WINDOWS;
	target.cpuArchitecture = data::gen::native::Target::X86;
	target.cpuMode = data::gen::native::Target::M32;
	target.cpuCapabilities = "";
#else
	target.platform = data::gen::native::Target::OTHER;
	target.cpuArchitecture = data::gen::native::Target::X86;
	target.cpuMode = data::gen::native::Target::M32;
	target.cpuCapabilities = "";
#endif
}

struct ClOption {
	const char* name;
	bool  requiresSecondParam;
	bool  wasUsed;
	const char* antipod;

	ClOption(const char* n) : name(n),requiresSecondParam(false),wasUsed(false),antipod(nullptr) {
	}
	ClOption(const char* n,const char* anti) : name(n),requiresSecondParam(false),wasUsed(false),antipod(anti) {
	}
	ClOption(const char* n,int extraParams) : name(n),requiresSecondParam(true),wasUsed(false),antipod(nullptr)  {
	}
};

char upper(const char c){
	if(c >= 'a' && c <= 'z') return c - ('a' - 'A');
	return c;
}
bool stringsEqualAnyCase(const char* str,const char* other){
	for(;;str++,other++){
		if(*str == '\0'){
			if(*other == '\0') return true;
			return false;
		}
		else if(*other== '\0') return false;

		if(*str != *other){
			if(upper(*str) != upper(*other)) return false;
		}
	}
	return true;
}
unittest(cl){
	assert(stringsEqualAnyCase("foo","foo"));
	assert(stringsEqualAnyCase("foo","FOO"));
	assert(stringsEqualAnyCase("foo1","FoO1"));
}

ClOption clOptions[]={ClOption("m32","m64"),ClOption("m64","m32"),ClOption("arch",1),ClOption("o",1)};
ClOption* findOption(const char* cl){
	auto end = clOptions+ (sizeof(clOptions) / sizeof(ClOption));
	for(auto i = clOptions;i!=end;i++){
		if(stringsEqualAnyCase(cl,i->name)){
			//verify antipod
			if(i->antipod){
				for(auto j = clOptions;j!=end;j++){
					if(stringsEqualAnyCase(i->antipod,j->name) && j->wasUsed){
						onWarning(format("The command line option '%s' is mutually exclusive with option '%s'! THe compiler will use the option '%s'.",i->name,j->name,i->name));
					}
				}
			}
			i->wasUsed = true;
			return i;
		}
	}
	return nullptr;
}

struct ClOptionApplier {
	data::Options* options;
	data::gen::Options* genOptions;
	data::gen::native::Target* nativeTarget;

	void paramError(const char* option,const char* param,const char* allowed = nullptr){
		onError(format("The parameter '%s' for the command line option '%s' is not valid!",param,option));
		if(allowed){
			System::print(format("        The allowed parameters are %s\n",allowed));
		}
	}

	void applyOption(const char* option,const char* param){
		using namespace data::gen;

		if(stringsEqualAnyCase(option,"m32"))      nativeTarget->cpuMode = native::Target::M32;
		else if(stringsEqualAnyCase(option,"m64")) nativeTarget->cpuMode = native::Target::M64;
		else if(stringsEqualAnyCase(option,"arch")){
			if(stringsEqualAnyCase(param,"x86")) nativeTarget->cpuArchitecture = native::Target::X86;
			else if(stringsEqualAnyCase(param,"arm")) nativeTarget->cpuArchitecture = native::Target::ARM;
			else paramError(option,param,"x86 or arm");
		}
		else if(stringsEqualAnyCase(option,"o")){
			if(stringsEqualAnyCase(param,"0")) genOptions->optimizationLevel = 0;
			else if(stringsEqualAnyCase(param,"1")) genOptions->optimizationLevel = 1;
			else if(stringsEqualAnyCase(param,"2")) genOptions->optimizationLevel = 2;
			else paramError(option,param,"0 or 1 or 2");
		}
	}
};


int main(int argc, const char * argv[]){
	
	System::init();
	memory::init();

	//initilize default settings
	const char* pp = "D:/alex/projects/parser/packages";
	data::Options options = { &pp,1 };

	data::gen::Options genOptions;
	genOptions.optimizationLevel = -1;
	genOptions.generate = true;

	data::gen::native::Target target;
	createDefaultTarget(target);

	//command line
	std::string operation;
	std::vector<std::string> files;
	bool hasErrors = false;
	if(argc >= 2){
		operation = argv[1]; 
		ClOptionApplier applier = {&options,&genOptions,&target};
		

		for(int i = 2;i<argc;i++){
			if(argv[i][0]=='-'){
				if(auto opt = findOption(&argv[i][1])){
					const char* optParam = nullptr;
					if(opt->requiresSecondParam){
						if(!((i+1) < argc)){
							onFatalError(format("The command line option '%s' requires another parameter after it!",opt->name));
						}
						optParam = argv[i+1];
						i++;
					}
					applier.applyOption(opt->name,optParam);
				}
				else onWarning(format("Unrecognized command line option '%s'! The compiler will ignore it!",argv[i]));
			}
			else {
				std::string file= argv[i];
				auto ext = System::path::extension(file.c_str());
				if(strcmp(ext,"") == 0){
					file += ".arp";
					ext = "arp";
				}

				if(!System::fileExists(file.c_str())){
					onError(format("The file '%s' doesn't exist!",file));
					hasErrors = true;
					continue;
				}
				if(strcmp(ext,"arp") && strcmp(ext,"obj") && strcmp(ext,"o") && strcmp(ext,"lib") && strcmp(ext,"a")){
					onError(format("The file '%s' is of unrecognized format!",file));
					hasErrors = true;
					continue;
				}
				files.push_back(file);
			}
		}
	}

	//initialize backend and frontend
	gen::LLVMBackend backend(&target,&genOptions);

	gen::Linker linker(&target,&genOptions);

	compiler::init(&options);
	//runTests();
	
	bool run  = false;
	bool link = true;
	if(operation == "run"){
		run = true;
		operation = "build";
	}
	else if(operation == "test"){
		//TODO
		operation = "build";
	}
	if(operation == "build"){
		if(files.size() < 1){
			onFatalError(format("No files provided!"));
		}
		compiler::reportLevel = compiler::ReportErrors;

		for(auto f = files.begin();f!=files.end();++f){
			auto file = (*f).c_str();

			if(strcmp(System::path::extension(file),"arp") == 0){
				auto module = compiler::newModuleFromFile(file);
				if(module->second.errorCount > 0){
					hasErrors = true;
					continue;
				}
				auto dir  = System::path::directory(file);
				auto name = System::path::filename(file);
				*f = backend.generateModule(module->second.body,dir.c_str(),name.c_str());
			}
		}
		if(hasErrors) return -1;

		if(compiler::generatedFunctions){

			files.push_back(backend.generateModule(compiler::generatedFunctions,"D:/Alex/projects/parser/build","gen"));
		}

		if(files.size() && link){
			std::vector<const char*> binaryFilesPtr(files.size(),nullptr);
			for(size_t i = 0;i < files.size();++i) binaryFilesPtr[i] = files[i].c_str();
			auto executable = linker.link(&binaryFilesPtr[0],binaryFilesPtr.size(),files[0].c_str(),data::gen::native::PackageLinkingFormat::EXECUTABLE);
		
			if(run) System::execute(executable.c_str(),"");
		}
	}
	if(argc < 2){
		System::print("\nWelcome to arpha code console. Type in the code and press return twice to compile it!\n");
		std::string source = "";//"import arpha.testing.testing\n";
		char buf[1024];
		while(true){
			std::cout<<"> ";
			std::cin.getline(buf,1024);
			if(buf[0]=='\0'){
				 auto mod = compiler::newModule("source",source.c_str());
				 if(compiler::generatedFunctions){
					debug("Generated functions - %s",compiler::generatedFunctions);
				 }

				 
				 auto srcf = backend.generateModule((*mod).second.body,"D:/Alex/projects/parser/build","src",data::gen::native::ASSEMBLY);
				 if(compiler::generatedFunctions){
					backend.generateModule(compiler::generatedFunctions,"D:/Alex/projects/parser/build","gen");
				 }
				 /*auto src = srcf.c_str();
				 linker.link(&src,1,"D:/Alex/projects/parser/build/src",data::gen::native::PackageLinkingFormat::EXECUTABLE);
				 */

				 source = "";
				 continue;
			}
			source+=buf;
			source+="\n";
		}
	}

	memory::shutdown();
	System::shutdown();
			
	return 0;
}
