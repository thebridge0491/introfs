# Targets Makefile script.
#----------------------------------------
# Common automatic variables legend (GNU make: make (Linux) gmake (FreeBSD)):
# $* - basename (cur target)  $^ - name(s) (all depns)  $< - name (1st depn)
# $@ - name (cur target)      $% - archive member name  $? - changed depns

CSC = mcs
FSC = fsharpc --consolecolors-

EXELAUNCHER ?= mono
NUGET ?= $(HOME)/bin/nuget.exe
#GENDARME ?= `find $(HOME)/nuget/packages -type f -iname gendarme.exe`
GENDARME ?= $(HOME)/nuget/packages/Mono.Gendarme/tools/gendarme.exe
MERGEAPP = $(HOME)/nuget/packages/ILRepack/tools/ILRepack.exe

ifeq ($(TESTFRWK),xunit)
TESTMONO_PATH = .:$(MONO_PATH):/usr/lib/mono/fsharp:/usr/local/lib/mono/fsharp:$(HOME)/nuget/packages/xunit/lib/net20:$(HOME)/nuget/packages/FsCheck/lib/net45:$(HOME)/nuget/packages/FsCheck.Xunit/lib/net45:$(HOME)/nuget/packages/FsUnit.xUnit/Lib/net40:build:$(OUTPUTPATH)
TESTCONSOLE = $(HOME)/nuget/packages/xunit.runners/tools/xunit.console.exe
TESTARGS =
refs_tests := $(refs_tests) $(shell $(PKG_CONFIG) --libs xunit) /r:FSharp.Core.dll /r:FsCheck/lib/net45/FsCheck.dll /r:FsCheck.Xunit/lib/net45/FsCheck.Xunit.dll

else
TESTMONO_PATH = .:$(MONO_PATH):/usr/lib/mono/fsharp:/usr/local/lib/mono/fsharp:$(HOME)/nuget/packages/NUnit.Runners/tools/lib:$(HOME)/nuget/packages/FsCheck.Nunit/lib/net45:$(HOME)/nuget/packages/FsCheck/lib/net45:$(HOME)/nuget/packages/FsUnit/Lib/Net40:build:$(OUTPUTPATH)
#TESTCONSOLE = `find $(HOME)/nuget/packages -type f -iname nunit-console.exe`
TESTCONSOLE = $(HOME)/nuget/packages/NUnit.Runners/tools/nunit-console.exe
TESTARGS = -nologo -domain=None -labels -xml=$(OUTPUTPATH)/TestResult.xml -output=$(OUTPUTPATH)/testout.txt -err=$(OUTPUTPATH)/testerrs.txt
refs_tests := $(refs_tests) $(shell $(PKG_CONFIG) --libs nunit.framework nunit.addinsdependencies) /r:FSharp.Core.dll /r:FsCheck/lib/net45/FsCheck.dll /r:FsCheck.Nunit/lib/net45/FsCheck.NUnit.Addin.dll /r:FsCheck.Nunit/lib/net45/FsCheck.NUnit.dll
endif

## combine products (assemblies or netmodules)
$(OUTPUTPATH)/$(proj).$(outext) : 
	-if [ ! "" = "$(OtherAssemblies)" ] ; then \
		(cd $(OUTPUTPATH) ; $(EXELAUNCHER) $(MERGEAPP) /t:$(outputtype) \
			/verbose $(keyfileopts) /xmldocs /out:$(proj).$(outext) \
			$(OtherAssemblies)) ; \
	elif [ ! "" = "$(AddModulesOpts)" ] ; then \
		$(CSC) $(CSCFLAGS) /t:$(outputtype) $(keyfileopts) $(refs_src) \
			$(resourceopts) /doc:$(OUTPUTPATH)/$(proj).xml $(startupopts) \
			/out:$@ $(AddModulesOpts) src/cs/properties/AssemblyInfo.cs ; \
	fi

$(OUTPUTPATH)/$(proj).CSharp.netmodule : $(src_cs)
	-if [ ! "" = "$^" ] ; then \
		$(CSC) $(CSCFLAGS) /t:module $(refs_src) \
			/doc:$(OUTPUTPATH)/$(proj).CSharp.xml /out:$@ $^ ; \
	fi
$(OUTPUTPATH)/$(proj).CSharp.dll : $(src_cs)
	-if [ ! "" = "$^" ] ; then \
		$(CSC) $(CSCFLAGS) /t:library $(keyfileopts) $(refs_src) \
			$(resourceopts) /doc:$(OUTPUTPATH)/$(proj).CSharp.xml \
			$(startupopts) /out:$@ $^ ; \
	fi

$(OUTPUTPATH)/$(proj).netmodule : $(src_fs)
	-if [ ! "" = "$^" ] ; then \
		$(FSC) $(FSCFLAGS) /target:module $(refs_src) \
			/sig:$(OUTPUTPATH)/$(proj).fsi \
			/doc:$(OUTPUTPATH)/$(proj).xml /out:$@ $^ ; \
	fi
$(OUTPUTPATH)/$(proj).$(outext) : $(src_fs)
	-if [ ! "" = "$(src_fs)" ] ; then \
		$(FSC) $(FSCFLAGS) /target:$(outputtype) $(keyfileopts) $(refs_src) \
			$(resourceopts) /sig:$(OUTPUTPATH)/$(proj).fsi \
			/doc:$(OUTPUTPATH)/$(proj).xml /out:$@ $^ ; \
	fi

$(OUTPUTPATH)/$(proj).Tests.dll : $(tests_cs) $(tests_fs)
	-if [ ! "" = "$(tests_fs)" ] ; then \
		$(FSC) $(FSCFLAGS) $(refs_tests) /target:library /out:$@ $(tests_fs) ; \
	else \
		$(CSC) $(CSCFLAGS) $(refs_tests) /t:library /out:$@ $(tests_cs) ; \
	fi


FMTS ?= tar.gz
distdir = $(proj).$(version)

.PHONY: help clean test testcompile dist monodoc lint monocover run debug valgrind nugetpack nugetadd nugetinstall
help: ## help
	@echo "##### subproject: $(proj) #####"
	@echo "Usage: $(MAKE) [target] -- some valid targets:"
#	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile Makefile-targets.mk ; fi` ; do \
#		grep -ve '^[A-Z]' $$fileX | awk '/^[^.%][-A-Za-z0-9_]+[ ]*:.*$$/ { print "...", substr($$1, 1, length($$1)) }' | sort ; \
#	done
	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile Makefile-targets.mk ; fi` ; do \
		grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $$fileX | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-25s%s\n", $$1, $$2}' ; \
	done
clean: ## clean build artifacts
	-rm -rf build/* build/.??*
testcompile: $(OUTPUTPATH)/$(proj).Tests.dll ## build test
test: testcompile ## run test [TOPTS=""]
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
	-LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):build MONO_PATH=$(TESTMONO_PATH) \
		MONO_GAC_PREFIX=$(HOME)/.local $(EXELAUNCHER) $(TESTCONSOLE) \
		$(TESTARGS) $(OUTPUTPATH)/$(proj).Tests.dll $(TOPTS)
dist: ## [FMTS="tar.gz"] archive source code
	-@mkdir -p build/$(distdir) ; cp -f exclude.lst build/
#	#-zip -9 -q --exclude @exclude.lst -r - . | unzip -od build/$(distdir) -
	-tar --format=posix --dereference --exclude-from=exclude.lst -cf - . | tar -xpf - -C build/$(distdir)
	
	-@for fmt in `echo $(FMTS) | tr ',' ' '` ; do \
		case $$fmt in \
			zip) echo "### build/$(distdir).zip ###" ; \
				rm -f build/$(distdir).zip ; \
				(cd build ; zip -9 -q -r $(distdir).zip $(distdir)) ;; \
			*) tarext=`echo $$fmt | grep -e '^tar$$' -e '^tar.xz$$' -e '^tar.bz2$$' || echo tar.gz` ; \
				echo "### build/$(distdir).$$tarext ###" ; \
				rm -f build/$(distdir).$$tarext ; \
				(cd build ; tar --posix -L -caf $(distdir).$$tarext $(distdir)) ;; \
		esac \
	done
	-@rm -r build/$(distdir)
monodoc: ## generate documentation
	-mdoc update -o build/doc_xmls -i $(OUTPUTPATH)/$(proj).xml \
		$(OUTPUTPATH)/$(proj).$(outext)
	-mdoc export-html --force-update -o build/docs build/doc_xmls
lint: ## lint check
	-$(EXELAUNCHER) $(GENDARME) --html build/lint_rpt.html \
		$(OUTPUTPATH)/$(proj).$(outext)
monocover: ## report code coverage
	-LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):build MONO_PATH=$(TESTMONO_PATH) \
		MONO_GAC_PREFIX=$(HOME)/.local $(EXELAUNCHER) --debug \
		--profile=coverage:output=build/cov.xml,covfilter-file=resources/covfilter.txt \
		--profile=log:output=build/cov.dat,coverage,covfilter-file=resources/covfilter.txt \
		$(TESTCONSOLE) $(TESTARGS) $(OUTPUTPATH)/$(proj).Tests.dll $(TOPTS)
	-mprof-report --reports=coverage build/cov.dat > build/cov.txt

DEBUGGER = gdb --args mono --debug		# lldb ; ddd --gdb; gdb
# valgrind tools: memcheck helgrind cachegrind massif lackey
VALGRIND = valgrind --verbose --tool=memcheck --suppressions=resources/mono.supp mono --debug

run: $(OUTPUTPATH)/$(proj).exe ## run main [ARGS=""]
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
	-if [ ! "" = "$(startupopts)" ] ; then \
		LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):build MONO_PATH=$(MONO_PATH) \
			MONO_GAC_PREFIX=$(HOME)/.local $(EXELAUNCHER) \
			$(OUTPUTPATH)/$(proj).exe $(ARGS) ; \
	fi
debug: $(OUTPUTPATH)/$(proj).exe ## debug main [ARGS=""]
	-if [ ! "" = "$(startupopts)" ] ; then \
		LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):build MONO_PATH=$(MONO_PATH) \
			MONO_GAC_PREFIX=$(HOME)/.local $(DEBUGGER) \
			$(OUTPUTPATH)/$(proj).exe $(ARGS) ; \
	fi
valgrind: $(OUTPUTPATH)/$(proj).exe ## valgrind main [ARGS=""]
	-if [ ! "" = "$(startupopts)" ] ; then \
		LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):build MONO_PATH=$(MONO_PATH) \
			MONO_GAC_PREFIX=$(HOME)/.local $(VALGRIND) \
			$(OUTPUTPATH)/$(proj).exe $(ARGS) ; \
	fi

build/$(proj).$(version).nupkg: $(proj).nuspec
	-rm -rf build/nupkg
	-mkdir -p build/nupkg/content build/nupkg/lib/net45 build/nupkg/build
	-cp -fR $(proj).nuspec LICENSE build/nupkg/
	-cp -fR LICENSE resources build/nupkg/content/
	-cp -fR $(OUTPUTPATH)/$(proj).dll $(OUTPUTPATH)/$(proj).?db \
		$(OUTPUTPATH)/$(proj).xml build/nupkg/lib/net45/
	-cp -fR $(OUTPUTPATH)/$(proj).exe build/nupkg/build/
#	-cp -fR src tests build/nupkg/
	-cd build/nupkg ; $(EXELAUNCHER) $(NUGET) pack -excludeemptydirectories \
		-outputdirectory .. $(proj).nuspec
nugetpack: $(proj).nuspec ## Nuget pack
nugetadd: build/$(proj).$(version).nupkg ## Nuget add
	-$(EXELAUNCHER) $(NUGET) add -source $(HOME)/.nuget/packages \
		build/$(proj).$(version).nupkg
nugetinstall: ## Nuget install
	-$(EXELAUNCHER) $(NUGET) install -source $(HOME)/.nuget/packages \
		-framework net45 -excludeversion -o $(HOME)/nuget/packages $(proj)
