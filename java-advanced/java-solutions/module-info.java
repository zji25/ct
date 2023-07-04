module info.kgeorgiy.ja.yarunina {
    requires transitive info.kgeorgiy.java.advanced.mapper;
    requires transitive info.kgeorgiy.java.advanced.concurrent;
    requires transitive info.kgeorgiy.java.advanced.implementor;
    requires transitive info.kgeorgiy.java.advanced.student;
    requires java.compiler;
    requires info.kgeorgiy.java.advanced.crawler;
    requires info.kgeorgiy.java.advanced.hello;
    requires java.rmi;
    requires jdk.httpserver;
    requires junit;
    exports info.kgeorgiy.ja.yarunina.i18n.tests;
    exports info.kgeorgiy.ja.yarunina.implementor;
    opens info.kgeorgiy.ja.yarunina.implementor to info.kgeorgiy.java.advanced.implementor;
}