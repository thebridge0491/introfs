#!/bin/sh

# usage: sh depns_dotnet.sh <func> [pkgsDir] [PREFIX]

nuget_install() {
	pkgsDir=${1:-$HOME/nuget/packages}
	mono $HOME/bin/nuget.exe install -framework net45 -excludeversion \
		-o $pkgsDir packages.config
	
	mono $HOME/bin/nuget.exe list -source $HOME/.nuget/packages ; sleep 5
}

# (re-sign assembly): sn -R AssemblyX.dll $(SN_TOKEN).snk
# to install delay signed assemblies to GAC (global assembly cache), 
#   edit [/usr/local]/etc/mono/X.Y/machine.config
#<configuration>
#  . .
#  <strongNames>
#    <verificationSettings>
#      <skip Token="$(SN_TOKEN)" Assembly="*" Users="*" />
#    </verificationSettings>
#    . .
#  </strongNames>
#</configuration>

gac_depns() {
	pkgsDir=${1:-$HOME/nuget/packages} ; PREFIX=${2:-$HOME/.local}
	
	PKG_CONFIG="pkg-config --with-path=${PREFIX}/lib/pkgconfig"
	pkgs_subpaths="nunit.framework:NUnit/lib/nunit.framework.dll xunit:xunit/lib/net20/xunit.dll nunit.addinsdependencies:NUnit.AddinsDependencies/lib/nunit.core.dll nunit.addinsdependencies:NUnit.AddinsDependencies/lib/nunit.core.interfaces.dll log4net:log4net/lib/net40-full/log4net.dll ini-parser:ini-parser/lib/INIFileParser.dll Newtonsoft.Json:Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll YamlDotNet:YamlDotNet/lib/net35/YamlDotNet.dll"
	pkgs=""
	
	for pkg_subpath in ${pkgs_subpaths} ; do
		pkgX=$(echo $pkg_subpath | cut -d: -f1) ;
		subpathX=$(echo $pkg_subpath | cut -d: -f2) ;
		gacutil -root ${PREFIX}/lib -package ${pkgX} -i $pkgsDir/${subpathX} ;
		pkgs="$pkgs ${pkgX}"
	done
	cp libdata/pkgconfig/*.pc ${PREFIX}/lib/pkgconfig/
	
	echo '' ; set -x
	gacutil -root ${PREFIX}/lib -l ; sleep 5
	${PKG_CONFIG} --path $pkgs ; sleep 5 ; set +x
}

func=$1 ; shift ;
${func} $@ ;

#--------------------------------------------------------------------
