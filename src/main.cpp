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
		const char* src;
	};

	typedef std::map<std::string,Module>::iterator ModulePtr;
	std::map<std::string,Module> modules;
	ModulePtr currentModule;

	std::string packageDir;

	std::map<std::string,void (*)(Scope*)> postCallbacks;


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
	}
	void onError(Node* node,const std::string& message){
		auto location = node->location();
		if(reportLevel >= ReportErrors)
			std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": Error: " << message << std::endl;
		
	}
	void intrinsicFatalError(Location& location,const std::string& message){
		std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": INTRINSIC FATAL ERROR: " << message << std::endl;
		std::cout<<"The compiler will now exit!"<<std::endl;
	}
	void headError(Location& location,const std::string& message){
		std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": Error: " << message << std::endl;
	}
	void subError(Location& location,const std::string& message){
		for(size_t i = 0;i<currentModule->first.size();i++) std::cout<<' ';
		std::cout<< '(' << location.line() << ':' << location.column << ')' <<": " << message << std::endl;
		showSourceLine(location,currentModule->first.size());
	}
	void onWarning(Location& location,const std::string& message){
		std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": Warning: " << message << std::endl;
		showSourceLine(location,currentModule->first.size());
	}


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

ClOption clOptions[]={ClOption("m32","m64"),ClOption("m64","m32"),ClOption("arch",1),ClOption("run")};
ClOption* findOption(const char* cl){
	auto end = clOptions+ (sizeof(clOptions) / sizeof(ClOption));
	for(auto i = clOptions;i!=end;i++){
		if(stringsEqualAnyCase(cl,i->name)){
			//verify antipod
			if(i->antipod){
				for(auto j = clOptions;j!=end;j++){
					if(stringsEqualAnyCase(i->antipod,j->name) && j->wasUsed){
						System::print(format("WARNING - The command line option '%s' is mutually exclusive with option '%s'! THe compiler will use the option '%s'.\n",i->name,j->name,i->name));
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
		System::print(format("ERROR - The parameter '%s' for the command line option '%s' is not valid!\n",param,option));
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
		else if(stringsEqualAnyCase(option,"run")) genOptions->run = true;

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
	genOptions.run = false;

	data::gen::native::Target target;
	createDefaultTarget(target);

	//command line
	std::string operation;
	std::vector<const char*> files;
	if(argc >= 2){
		operation = argv[1]; 
		ClOptionApplier applier = {&options,&genOptions,&target};
		

		for(int i = 2;i<argc;i++){
			if(argv[i][0]=='-'){
				if(auto opt = findOption(&argv[i][1])){
					const char* optParam = nullptr;
					if(opt->requiresSecondParam){
						if(!((i+1) < argc)){
							System::print(format("ERROR - the command line option '%s' requires another parameter after it!\n",opt->name));
							return -1;
						}
						optParam = argv[i+1];
						i++;
					}
					applier.applyOption(opt->name,optParam);
				}
				else System::print(format("WARNING - unrecognized command line option '%s'! The compiler will ignore it!\n",argv[i]));
			}
			else {
				files.push_back(argv[i]);
				System::print(argv[i]);
				System::print("\n");
			}
		}
	}

	//initialize backend and frontend
	gen::LLVMBackend backend(&target,&genOptions);

	gen::Linker linker(&target,&genOptions);

	compiler::init(&options);
	//runTests();
	
	if(operation == "build"){
		auto mod = compiler::newModuleFromFile(files[0]);

		auto dir = System::path::directory(files[0]);
		auto name = System::path::filename(files[0]);

		auto srcf = backend.generateModule((*mod).second.body,dir.c_str(),name.c_str());
		auto src = srcf.c_str();
		auto executable = linker.link(&src,1,files[0],data::gen::native::PackageLinkingFormat::EXECUTABLE);
		
		if(genOptions.run)
			System::execute(executable.c_str(),"");
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

				 
				 auto srcf = backend.generateModule((*mod).second.body,"D:/Alex/projects/parser/build","src");
				 if(compiler::generatedFunctions){
					backend.generateModule(compiler::generatedFunctions,"D:/Alex/projects/parser/build","gen");
				 }
				 auto src = srcf.c_str();
				 linker.link(&src,1,"D:/Alex/projects/parser/build/src",data::gen::native::PackageLinkingFormat::EXECUTABLE);

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
