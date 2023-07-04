#!/bin/bash
path="../java-advanced-2023/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/"
cd ..
javadoc -d javadoc \
-link https://docs.oracle.com/en/java/javase/20/docs/api/ \
-private \
-cp ../java-advanced-2023/lib:../java-advanced-2023/artifacts \
info/kgeorgiy/ja/yarunina/implementor/Implementor.java \
"$path"Impler.java "$path"JarImpler.java "$path"ImplerException.java
cd scripts
