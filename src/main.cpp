/**
* Arpha programming language compiler.
* (c) 2012
*/
#include <algorithm>

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

	BlockExpression* parseModule(Parser* parser,BlockExpression* block);
};

void runTests();

namespace {
	System::OutputBuffer dumpToConsole;
	bool   dumpToConsoleInited = false;
}
Dumper Dumper::console(){
	if(!dumpToConsoleInited ){
		dumpToConsole = System::console();
		dumpToConsoleInited = true;
	}

	Dumper dumper;
	dumper.destination = dumpToConsole;
	dumper.indentation = 0;
	return dumper;
}

Dumper::Dumper(std::ostream* stream) {
	this->stream = stream;
	flags = IS_STREAM;
}
void Dumper::print(const char* str){
	if((flags & IS_STREAM)!=0) (*stream)<<str;
	else destination.print(str);
}
void Dumper::printIndentation(){
	for(auto i = 0;i<indentation;++i) print("  ");
}
void Dumper::incIndentation(){
	indentation++;
}
void Dumper::decIndentation(){
	indentation--;
}

namespace compiler {

	struct Package;

	struct Module {
		std::string directory;
		Scope* scope;
		Node*  body;
		std::map<std::string,Package>::iterator package;
		size_t errorCount;
		const char* src;
	};
	typedef std::map<std::string,Module>::iterator ModulePtr;
	
	std::map<std::string,Module> modules;
	ModulePtr currentModule;

	struct Package {
		std::vector<ModulePtr> modules;
	};
	typedef std::map<std::string,Package>::iterator PackagePtr;
	std::map<std::string,Package> packages;

	std::string packageDir;
	const char** rootImportDirectory;
	size_t rootImportDirectoryCount;

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
		if(!generatedFunctions){
			generatedFunctions = new BlockExpression;
			generatedFunctions->label("__gen");
		}
		generatedFunctions->addChild(expr);
	}

	BlockExpression* generatedFunctions = nullptr;
	Function* generatedTestMain = nullptr;
	bool testing = false;

	void addFunctionToTestsuite(Function* function){
		if(!testing) return;
		if(!generatedTestMain){
			generatedTestMain = new Function("main",Location());
			generatedTestMain->specifyReturnType(intrinsics::types::Void);
			generatedTestMain->setFlag(Node::RESOLVED);
			addGeneratedExpression(generatedTestMain);
			generatedTestMain->parentNode = generatedFunctions;
		}
		auto call = new CallExpression(new FunctionReference(function),new UnitExpression());
		call->setFlag(Node::RESOLVED);
		generatedTestMain->body.addChild(call);
	}


	ModulePtr newModule(const char* path,const char* source,PackagePtr* package= nullptr){
		Module module = {};
		auto insertionResult = modules.insert(std::make_pair(std::string(path),module));

		auto prevModule = currentModule;
		currentModule = insertionResult.first;

		currentModule->second.directory = System::path::directory(path);
		if(package) currentModule->second.package = *package;
		else currentModule->second.package = packages.end();
		currentModule->second.errorCount = 0;
		currentModule->second.src = source;

		//module
		auto block = new BlockExpression();
		//Special case for 'packages/arpha/arp.arp'
		if((packageDir + "/arpha/arpha.arp") == path){
			//import 'arpha' by default
			arpha::defineCoreSyntax(block->scope);
			compiler::registerResolvedIntrinsicModuleCallback("arpha/types",intrinsics::types::preinit);
		}
		else {
			auto cb = postCallbacks.find(path);
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
		
		arpha::parseModule(&parser,block);

		dumpModule(block);
		resolver.resolveModule(block);

		currentModule->second.src = nullptr;
		//restore old module ptr
		currentModule = prevModule;
		_currentUnit = prevUnit;
		return insertionResult.first;
	}

	ModulePtr newModuleFromFile(const char* filename,const char* moduleName = nullptr,PackagePtr* package= nullptr){
		std::string m;
		if(!moduleName){
			m = System::path::filename(filename);
			auto index = m.find_last_of('.');
			m[index] = '_';
			moduleName = m.c_str();
		}
		auto src = System::fileToString(filename);
		auto module = newModule(filename,(const char*)src,package);
		System::free((void*)src);
		module->second.body->label(moduleName);
		return module;
	}

	//Module importing is done by searching in the appropriate directories
	Scope* findModuleFromDirectory(const char* dir,const char* name,ModulePtr* relative = nullptr){
		std::string moduleName;
		//Try non package way
		auto filename = std::string(dir) + "/" + name + ".arp";
		if(!System::fileExists(filename.c_str())){
			//Try package way
			filename = std::string(dir) + "/" + name + "/" + System::path::filename(name) + ".arp";
			if(!System::fileExists(filename.c_str())) return nullptr;
			else moduleName = std::string(name) + "_" + name;
		} else moduleName = name;
		//load module
		auto module = modules.find(filename);
		if(module == modules.end()){
			std::replace(moduleName.begin(),moduleName.end(),'/','_');
			PackagePtr package;
			if(!relative){
				auto comp = System::path::firstComponent(&name); //'arpha/foo' -> 'arpha'
				auto packagePath = std::string(dir) + "/" + std::string(comp.first,comp.second); //'packages/arpha'
				
				auto insertionResult = packages.insert(std::make_pair(packagePath,Package()));
				package = insertionResult.first;
			}	else {
				package = (*relative)->second.package;
				if(package!=packages.end()){
					moduleName = System::path::filename(package->first.c_str()) + "_" + moduleName;//TODO proper
				}
			}

			onDebug(format("A new module %s located at '%s' will be loaded.",name,filename));
			module = newModuleFromFile(filename.c_str(),moduleName.c_str(),&package);

			package->second.modules.push_back(module);
		}
		return module->second.scope;
	}

	//Finds a module and loads it if necessary to match the existing name
	Scope* findModule(const char* name){
		//Search in the current directory
		if(currentModule != modules.end()){
			auto module = findModuleFromDirectory(currentModule->second.directory.c_str(),name,&currentModule);
			if(module) return module;
		}
		
		//Search in the packages directory for a package
		for(auto i = rootImportDirectory; i!= rootImportDirectory + rootImportDirectoryCount;++i){
			auto module = findModuleFromDirectory(*i,name,nullptr);
			if(module) return module;
		}
	}

	int reportLevel;

	void init(data::Options* options){
		interpreter = constructInterpreter(nullptr);
		currentModule = modules.end();

		rootImportDirectory      = options->packagesPaths;
		rootImportDirectoryCount = options->packagesPathsCount;
		assert(rootImportDirectoryCount);

		packageDir = rootImportDirectory[0];

		reportLevel = ReportErrors;

		intrinsics::types::startup();

		//Load language definitions.
		findModuleFromDirectory(packageDir.c_str(),"arpha/arpha",nullptr);
		//newModuleFromFile((packageDir + "/arpha/arpha.arp").c_str(),"arpha_arpha",nullptr,true);

		reportLevel = ReportDebug;
	}

	void dumpModule(Node* module){
		if(reportLevel >= ReportDebug){
			auto console = Dumper::console();
			console.flags |= Dumper::VERBOSE | Dumper::REF_DECL_POINTERS;
			console.print("------------------- AST: ------------------------------\n");
			module->dump(console);
			console.print("\n");
		}
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
			System::print(format("\t%s(%d:%d): ",module->first,function->location().lineNumber,function->location().column));
			//for(auto i = function->arguments.begin();i!=function->arguments.end();++i)
			//	System::print(format("%s %s%c ",(*i)->label(),(*i)->type,(i+1)==function->arguments.end()? ')':','));

			auto con = Dumper::console();
			function->dumpDeclaration(con);
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

ClOption clOptions[]={ClOption("m32","m64"),ClOption("m64","m32"),ClOption("arch",1),ClOption("o",1),ClOption("asm"),ClOption("llvmbc")};
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
	int* outputFormat;

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
		else if(stringsEqualAnyCase(option,"asm"))    *outputFormat |= data::gen::native::ASSEMBLY;
		else if(stringsEqualAnyCase(option,"llvmbc")) *outputFormat |= gen::LLVMBackend::OUTPUT_BC;
	}
};

void buildPackages(gen::LLVMBackend& backend,std::vector<std::string>& files){
	std::vector<Node*> modules;
	for(auto i = compiler::packages.begin(); i!=compiler::packages.end();++i){
		
		System::print(format("Compiling the package '%s' for the first time... \n",i->first));
		
		auto moduleCount = i->second.modules.size();
		modules.resize(moduleCount);
		for(size_t j = 0;j<moduleCount;j++) modules[j] = i->second.modules[j]->second.body;
		files.push_back(backend.generateModule(modules.begin()._Ptr,modules.size(),i->first.c_str(),"arpha_cache"));
		modules.clear();
	}
}

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
	int outputFormat = data::gen::native::OBJECT;
	if(argc >= 2){
		operation = argv[1]; 
		ClOptionApplier applier = {&options,&genOptions,&target,&outputFormat};
		

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
		run = true;
		compiler::testing = true;
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
				*f = backend.generateModule(module->second.body,dir.c_str(),name.c_str(),outputFormat);
			}
		}
		if(hasErrors) return -1;

		buildPackages(backend,files);
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

				mod->second.body->label("source");
				auto srcf = backend.generateModule((*mod).second.body,"D:/Alex/projects/parser/build","src",data::gen::native::ASSEMBLY);
				buildPackages(backend,files);
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
