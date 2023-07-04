#!/bin/bash
rm implementor.jar
cd ..
javac -d scripts --module-path ../java-advanced-2023/lib:../java-advanced-2023/artifacts \
	module-info.java info/kgeorgiy/ja/yarunina/implementor/Implementor.java
cd scripts
jar -cfm implementor.jar MANIFEST.MF info/kgeorgiy/ja/yarunina/implementor/Implementor.class module-info.class
rm -r info
rm module-info.class