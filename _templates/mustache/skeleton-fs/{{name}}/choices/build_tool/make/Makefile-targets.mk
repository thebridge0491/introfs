# Targets Makefile script.
#----------------------------------------
# Common automatic variables legend (GNU make: make (Linux) gmake (FreeBSD)):
# $* - basename (cur target)  $^ - name(s) (all depns)  $< - name (1st depn)
# $@ - name (cur target)      $% - archive member name  $? - changed depns

FMTS ?= tar.gz,zip
distdir = $(proj).$(version)

##TESTCONSOLEAPP = $(HOME)/.nuget/packages/xunit.runner.console/*/tools/net452/xunit.console.exe
##TESTAPPARGS = -nologo -appdomains denied -xml $(OUTPUTPATH)/TestResult.xml
#TESTCONSOLEAPP = `find $(HOME)/.nuget/packages/nunit.consolerunner -type f -iname nunit3-console.exe`
TESTCONSOLEAPP = $(HOME)/.nuget/packages/nunit.consolerunner/*/tools/nunit3-console.exe
TESTAPPARGS = --noheader --domain=None --labels=OnOutputOnly --output=$(OUTPUTPATH)/testout.txt "--result=$(OUTPUTPATH)/TestResult.xml;format=nunit3"

#GENDARMEAPP = `find $(HOME)/.nuget/packages/mono.gendarme -type f -iname gendarme.exe`
GENDARMEAPP = $(HOME)/.nuget/packages/mono.gendarme/*/tools/gendarme.exe
ILREPACKAPP = $(HOME)/.nuget/packages/ilrepack/*/tools/ILRepack.exe

build/$(distdir) :
	-@mkdir -p build/$(distdir) ; cp -f exclude.lst build/
#	#-zip -9 -q --exclude @exclude.lst -r - . | unzip -od build/$(distdir) -
	-tar --format=posix --dereference --exclude-from=exclude.lst -cf - . | tar -xpf - -C build/$(distdir)
	
.PHONY: help clean check dist monodoc lint monocover nugetadd nugetinstall

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
check: testcompile ## run test [TOPTS=""]
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
	-LD_LIBRARY_PATH=$(OUTPUTPATH):$(LD_LIBRARY_PATH) MONO_PATH=$(OUTPUTPATH):$(MONO_PATH) \
		MONO_GAC_PREFIX=$(HOME)/.local $(EXELAUNCHER) $(TESTCONSOLEAPP) \
		$(TESTAPPARGS) $(OUTPUTPATH)/$(proj).Tests.dll $(TOPTS)
nugetadd: bin/$(CONFIG)/$(proj).$(version).nupkg ## Nuget add
	-$(EXELAUNCHER) $(NUGETAPP) add -source $(HOME)/.nuget/packages \
		bin/$(CONFIG)/$(proj).$(version).nupkg
nugetinstall: ## Nuget install
	-$(EXELAUNCHER) $(NUGETAPP) install -source $(HOME)/.nuget/packages \
		-framework $(FRAMEWORK) -excludeversion -o $(HOME)/nuget/packages $(proj)
	-$(EXELAUNCHER) $(NUGETAPP) search -source $(HOME)/.nuget/packages $(proj)
	-cp src/`echo $(proj) | tr 'A-Z' 'a-z'`.pc.in $(HOME)/.local/lib/pkgconfig/`echo $(proj) | tr 'A-Z' 'a-z'`.pc
	-sh -xc "$(PKG_CONFIG) --list-all | grep `echo $(proj) | tr 'A-Z' 'a-z'`"

dist: | build/$(distdir) ## [FMTS="tar.gz,zip"] archive source code
	-@for fmt in `echo $(FMTS) | tr ',' ' '` ; do \
		case $$fmt in \
			7z) echo "### build/$(distdir).7z ###" ; \
				rm -f build/$(distdir).7z ; \
				(cd build ; 7za a -t7z -mx=9 $(distdir).7z $(distdir)) ;; \
			zip) echo "### build/$(distdir).zip ###" ; \
				rm -f build/$(distdir).zip ; \
				(cd build ; zip -9 -q -r $(distdir).zip $(distdir)) ;; \
			*) tarext=`echo $$fmt | grep -e '^tar$$' -e '^tar.xz$$' -e '^tar.zst$$' -e '^tar.bz2$$' || echo tar.gz` ; \
				echo "### build/$(distdir).$$tarext ###" ; \
				rm -f build/$(distdir).$$tarext ; \
				(cd build ; tar --posix -h -caf $(distdir).$$tarext $(distdir)) ;; \
		esac \
	done
	-@rm -r build/$(distdir)
monodoc: ## generate documentation
	-mdoc update -o build/doc_xmls -i $(OUTPUTPATH)/$(proj).xml \
		$(OUTPUTPATH)/$(proj).$(outext)
	-mdoc export-html --force-update -o build/docs build/doc_xmls
lint: ## lint check
	-$(EXELAUNCHER) $(GENDARMEAPP) --html build/lint_rpt.html \
		$(OUTPUTPATH)/$(proj).$(outext)
monocover: ## report code coverage
	-LD_LIBRARY_PATH=$(OUTPUTPATH):$(LD_LIBRARY_PATH) MONO_PATH=$(OUTPUTPATH):$(MONO_PATH) \
		MONO_GAC_PREFIX=$(HOME)/.local $(EXELAUNCHER) --debug -O=-aot \
		--profile=coverage:output=build/cov.xml,covfilter-file=resources/covfilter.txt \
		--profile=log:output=build/cov.dat,covfilter-file=resources/covfilter.txt \
		$(TESTCONSOLEAPP) $(TESTAPPARGS) $(OUTPUTPATH)/$(proj).Tests.dll $(TOPTS)
	-mprof-report --out=build/cov.txt build/cov.dat

#DEBUGGER = gdb --args mono --debug		# lldb ; ddd --gdb; gdb
## valgrind tools: memcheck helgrind cachegrind massif lackey
#VALGRIND = valgrind --verbose --tool=memcheck --suppressions=resources/mono.supp mono --debug
