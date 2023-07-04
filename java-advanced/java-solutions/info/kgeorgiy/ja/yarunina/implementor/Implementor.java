package info.kgeorgiy.ja.yarunina.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.*;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;

/**
 * Generator of {@code .java} and {@code .jar} files for class implementation by {@link Class type token}
 * of class or interface. Implementation of {@link info.kgeorgiy.java.advanced.implementor.Impler} and
 * {@link info.kgeorgiy.java.advanced.implementor.JarImpler} interfaces.
 */
public class Implementor implements Impler, JarImpler {
    /**
     * Contains a {@link System#lineSeparator()} value: the system-dependent line separator string.
     */
    private static final String LINE_SEPARATOR = System.lineSeparator();
    /**
     * Contains a {@link File#separatorChar} value: the system-dependent default name-separator character.
     */
    private static final char FILE_SEPARATOR = File.separatorChar;

    /**
     * Contains a string value for extension of a java file.
     */
    private static final String JAVA_EXTENSION = ".java";
    /**
     * Contains a string value for extension of a java class file.
     */
    private static final String CLASS_EXTENSION = ".class";
    /**
     * Contains a value for the standard encoding.
     */
    private static final Charset ENCODING = StandardCharsets.UTF_8;

    /**
     * Format string for implementing package description information.
     */
    private static final String PACKAGE_FORMAT;
    /**
     * Format string for implementing the declaration of class.
     */
    private static final String DECLARATION_FORMAT;
    /**
     * Format string for implementing the constructor.
     */
    private static final String CONSTRUCTOR_FORMAT;
    /**
     * Format string for implementing the method.
     */
    private static final String METHOD_FORMAT;
    /**
     * Format string for implementing information about exceptions a constructor or a method throws.
     */
    private static final String EXCEPTIONS_FORMAT;
    /**
     * Format string for implementing the return statement of a method.
     */
    private static final String RETURN_FORMAT;

    static {
        PACKAGE_FORMAT = String.format("package %%s;%s%s", LINE_SEPARATOR, LINE_SEPARATOR);
        DECLARATION_FORMAT = String.format("class %%sImpl %%s %%s {%s", LINE_SEPARATOR);
        CONSTRUCTOR_FORMAT = String.format("\t%%s %%sImpl%%s %%s{%s\t\tsuper(%%s);%s\t}%s%s",
                LINE_SEPARATOR, LINE_SEPARATOR, LINE_SEPARATOR, LINE_SEPARATOR);
        METHOD_FORMAT = String.format("\t%%s %%s %%s%%s %%s{%%s}%s%s", LINE_SEPARATOR, LINE_SEPARATOR);
        EXCEPTIONS_FORMAT = String.format("%s\t\t\tthrows %%s ", LINE_SEPARATOR);
        RETURN_FORMAT = String.format("%s\t\treturn %%s;%s\t", LINE_SEPARATOR, LINE_SEPARATOR);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void implement(final Class<?> token, final Path root) throws ImplerException {
        checkNonNull(token, "token");
        checkNonNull(root, "root path");

        final int mod = token.getModifiers();
        if (token.isArray() || token.isPrimitive() || token.isEnum() || token.equals(Enum.class)
                || Modifier.isFinal(mod) || Modifier.isPrivate(mod)) {
            throw new ImplerException("provided token represents an unimplementable type");
        }

        final Path path = root.resolve(Path.of(getFileName(token, FILE_SEPARATOR, JAVA_EXTENSION)));

        try {
            if (path.getParent() != null) Files.createDirectories(path.getParent());
            Files.deleteIfExists(path);
            Files.createFile(path);
        } catch (final IOException e) {
            throw new ImplerException("can't create file or its directories", e);
        }

        try (final BufferedWriter writer = Files.newBufferedWriter(path, ENCODING)) {
            implementCode(token, writer);
        } catch (final IOException e) {
            throw new ImplerException("error while writing to generated file", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        checkNonNull(token, "token");
        checkNonNull(jarFile, "jar file path");

        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) throw new ImplerException("this platform doesn't provide a java compiler");
        final Path temp = Paths.get(".");

        implement(token, temp);

        final String classpath;
        try {
            classpath = Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new ImplerException("error converting URL to URI", e);
        }
        if (0 != compiler.run(null, null, null,
                "-encoding", ENCODING.name(), "-cp", classpath, getFileName(token, FILE_SEPARATOR, JAVA_EXTENSION))) {
            throw new AssertionError("compilation failed");
        }

        final Manifest manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
        try (final JarOutputStream stream = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
            final String className = getFileName(token, '/', CLASS_EXTENSION);
            stream.putNextEntry(new ZipEntry(className));
            Files.copy(temp.resolve(className), stream);
        } catch (final IOException e) {
            throw new ImplerException("error writing files to jar", e);
        }
    }

    /**
     * Checks if the provided Object is {@code null}.
     *
     * @param object {@link Object} to check.
     * @param type   {@link String} that describes the obj.
     * @throws ImplerException if the object is null.
     */
    private void checkNonNull(final Object object, final String type) throws ImplerException {
        if (object == null) throw new ImplerException(String.format("%s can't be null", type));
    }

    /**
     * Generates a file path string based on its package, class name and extension, joined with provided separator.
     *
     * @param token     type token to get file name of.
     * @param separator separator char.
     * @param extension {@link String} extension for the path.
     * @return {@code String} of th generated file path.
     */
    private String getFileName(final Class<?> token, final char separator, final String extension) {
        return String.format("%s%c%sImpl%s",
                token.getPackageName().replace('.', separator), separator, token.getSimpleName(), extension);
    }


    /**
     * Generates code for a provided type token and writes in to the given {@link BufferedWriter}.
     *
     * @param token  type token to create implementation for.
     * @param writer {@link BufferedWriter} to write generated code to.
     * @throws IOException     if an I/O error occurs while writing
     * @throws ImplerException if token represents an unimplementable class:
     *                         all declared constructors are private
     */
    private void implementCode(final Class<?> token, final BufferedWriter writer) throws IOException, ImplerException {
        final String packageName = token.getPackageName();
        final String tokenName = token.getSimpleName();

        if (!packageName.isEmpty()) writer.write(String.format(PACKAGE_FORMAT, packageName));

        writer.write(String.format(DECLARATION_FORMAT,
                tokenName, (token.isInterface() ? "implements" : "extends"), token.getCanonicalName()));

        if (!token.isInterface()) {
            final Set<Constructor<?>> constructors = getConstructors(token);
            if (constructors.isEmpty()) throw new ImplerException("can't implement a class without constructors");
            for (final Constructor<?> constructor : constructors) implementConstructor(constructor, tokenName, writer);
        }
        for (final Method method : getMethods(token)) implementMethod(method, writer);
        writer.write("}");
    }


    /**
     * Finds all implementable constructors of the provided {@link Class}.
     * That includes all constructors implicitly or explicitly declared by the class represented by this Class,
     * excluding the private constructors. These are public, protected and default (package) access constructors.
     *
     * @param token a {@link Class} to find constructors of.
     * @return {@link Set} of implementable constructors.
     */
    private Set<Constructor<?>> getConstructors(final Class<?> token) {
        return Arrays.stream(token.getDeclaredConstructors())
                .filter(x -> !Modifier.isPrivate(x.getModifiers()))
                .collect(Collectors.toSet());
    }

    /**
     * Finds all implementable methods of the provided {@link Class}.
     * <ul>
     *     <li>If {@code token} represents an interface type, returns all the public methods of the interface
     *     represented by {@code token}, including declared by the interface and inherited from superinterfaces.
     *     Does not contain any implicitly declared methods from {@code token}.</li>
     *     <li>If {@code token} represents a class type, returns all the non-private non-final abstract methods
     *     of the class represented by {@code token}, including inherited from superclasses and superinterfaces.</li>
     * </ul>
     *
     * @param token a Class to find methods of.
     * @return Set of implementable methods.
     */
    private Set<Method> getMethods(final Class<?> token) {
        final Set<MethodWrapper> set = new HashSet<>();
        for (Class<?> t = token; t != null; t = t.getSuperclass()) addMethods(set, t.getDeclaredMethods());
        assert token != null;
        addMethods(set, token.getMethods());
        return set.stream()
                .map(MethodWrapper::method)
                .collect(Collectors.toSet());
        // :NOTE: interface A { Object kek(); }
        //        interface B extends A { String kek(); } fixed ig
    }

    /**
     * Finds all implementable methods from the provided List and adds them to the given Set.
     * Returned Set includes all abstract non-final non-private methods, excluding those that are default.
     *
     * @param set     {@link Set} to add methods to.
     * @param methods {@link List} of implementable methods.
     */
    private static void addMethods(final Set<MethodWrapper> set, final Method[] methods) {
        Arrays.stream(methods)
                .filter(x -> {
                    final int mod = x.getModifiers();
                    return Modifier.isAbstract(mod) && !Modifier.isFinal(mod) && !Modifier.isPrivate(mod)
                            && !x.isDefault(); // :NOTE: default methods in interfaces fixed
                })
                .map(MethodWrapper::new)
                .collect(Collectors.toCollection(() -> set));
    }


    /**
     * Generates code for the provided {@link Constructor} of a {@link Class}
     * and writes in to the given {@link BufferedWriter}.
     *
     * @param constructor the {@link Constructor} to implement.
     * @param name        {@link String} name of the class as given in the source code.
     * @param writer      {@link BufferedWriter} to write generated code to.
     * @throws IOException if an I/O error occurs while writing.
     */
    private void implementConstructor(final Constructor<?> constructor, final String name, final BufferedWriter writer)
            throws IOException {
        writer.write(String.format(CONSTRUCTOR_FORMAT,
                getModifiersString(constructor), name, getParametersString(constructor),
                getExceptionTypesString(constructor),
                Arrays.stream(constructor.getParameters())
                        .map(Parameter::getName)
                        .collect(Collectors.joining(", "))));
    }

    /**
     * Generates code for the provided {@code Method} and writes in to the given {@code BufferedWriter}.
     *
     * @param method the {@link Method} to implement.
     * @param writer {@link BufferedWriter} to write generated code to.
     * @throws IOException if an I/O error occurs while writing.
     */
    private void implementMethod(final Method method, final BufferedWriter writer) throws IOException {
        final Class<?> returnType = method.getReturnType();
        writer.write(String.format(METHOD_FORMAT,
                getModifiersString(method), returnType.getCanonicalName(), method.getName(),
                getParametersString(method), getExceptionTypesString(method),
                getReturnString(returnType)));
    }


    /**
     * Generates code of the provided {@code Executable} class' modifiers
     * excluding {@code abstract} and {@code transient} modifiers.
     * If this {@code Executable} was declared to take a variable number of arguments,
     * its modifiers contain {@code transient}, so it removes it, since {@code Executable} can't be {@code transient}.
     *
     * @param executable {@link Executable} class to get modifiers of.
     * @return {@link String} of the acceptable modifiers of the provided {@code Executable} class.
     */
    private String getModifiersString(final Executable executable) {
        final int mod = executable.getModifiers();
        assert Modifier.isTransient(mod) == executable.isVarArgs();
        return Modifier.toString(mod
                - (Modifier.isAbstract(mod) ? Modifier.ABSTRACT : 0)
                - (executable.isVarArgs() ? Modifier.TRANSIENT : 0)); // :NOTE: тут на самом деле не transient // но как бы и transient
    }

    /**
     * Generates code for the provided {@code Executable} class' parameters.
     *
     * @param executable {@link Executable} class to get parameters of.
     * @return {@link String} of all the parameters list of a {@code executable}.
     * Each parameter is represented by its type's canonical name and its name.
     * @see Class#getCanonicalName()
     * @see Parameter#getName()
     */
    private String getParametersString(final Executable executable) {
        return String.format("(%s)", Arrays.stream(executable.getParameters())
                .map(x -> String.format("%s %s", x.getType().getCanonicalName(), x.getName()))
                .collect(Collectors.joining(", ")));
    }

    /**
     * Generates code for the provided Executable class' exception types.
     *
     * @param executable {@link Executable} class to get exception types of.
     * @return {@link String} of all the exceptions {@code executable} may throw.
     * Each exception type is represented by its name.
     */
    private String getExceptionTypesString(final Executable executable) {
        final Class<?>[] exceptions = executable.getExceptionTypes();
        return exceptions.length == 0 ? "" : String.format(EXCEPTIONS_FORMAT,
                Arrays.stream(exceptions)
                        .map(Class::getCanonicalName)
                        .collect(Collectors.joining(", ")));
    }

    /**
     * Generates a code for the return statement String for a method.
     *
     * @param returnType {@link Class} of the type of the method's return value.
     * @return {@link String} of the return statement.
     * If the provided return type is {@code void}, returns an empty string.
     * Otherwise, returns a string containing {@code return} keyword and the default value for a given type.
     */
    private String getReturnString(final Class<?> returnType) {
        return (returnType.equals(void.class) ? "" :
                String.format(RETURN_FORMAT,
                        returnType.equals(boolean.class) ? "false" : (returnType.isPrimitive() ? "0" : "null")));
    }


    /**
     * A wrapper for the {@code Method} to override their {@link Method#equals} and {@link Method#hashCode} methods.
     * Only compares {@code methods} by their names and formal parameter types,
     * ignoring the declaring class and return type.
     *
     * @param method a {@link Method} to wrap.
     * @see Method#equals(Object)
     * @see Method#hashCode()
     */
    private record MethodWrapper(Method method) {
        /**
         * Compares this MethodWrapper against the specified object. Returns true if the objects are equal.
         * Two MethodWrappers are equal if their Methods have the same name, formal parameter types
         * and the return type.
         *
         * @param obj the reference {@link Object} with which to compare.
         * @return {@code true} if this method is equal to the obj argument; {@code false} otherwise
         */
        @Override
        public boolean equals(final Object obj) {
            if (obj instanceof MethodWrapper other) {
                final Method otherMethod = other.method();
                return method.getName().equals(otherMethod.getName())
                        && Arrays.equals(method.getParameterTypes(), otherMethod.getParameterTypes());
            }
            return false;
        }

        /**
         * Calculates a hashcode for this {@code MethodWrapper}.
         * The hashcode is computed as the hash of the hashcodes for the underlying method's name and parameter types.
         *
         * @return {@code int} calculated hashcode.
         */
        @Override
        public int hashCode() {
            return Objects.hash(method.getName(), Arrays.hashCode(method.getParameterTypes()));
        }
    }


    /**
     * Command line interface for {@code Implementor}.
     * <ul>
     *     <li>In the first mode, if two arguments are provided specifying a {@code Class<?> token} and
     *     a directory, generates an implementation of the {@code token} and places it in a given directory,
     *     using {@link Implementor#implement(Class, Path)}.</li>
     *     <li>In the second mode, if the first argument is {@code String} "-jar", and next two arguments are same
     *     as before, generates an implementation of the {@code token} and places it in a given directory
     *     in a ".jar" file, using {@link Implementor#implementJar(Class, Path)}.</li>
     * </ul>
     *
     * @param args the command line parameters.
     */
    public static void main(final String[] args) {
        if (args == null || !(args.length == 2 || args.length == 3 && args[0].equals("-jar"))) {
            System.err.println("provided arguments are not supported: " +
                    "has to include \"-jar\" (optional), class/interface to implement and path for the generated file.");
            return;
        }
        final boolean jar = args.length == 3;
        try {
            final Class<?> token = Class.forName(args[jar ? 1 : 0]);
            final Path root = Path.of(args[jar ? 2 : 1]);
            final Implementor implementor = new Implementor();
            if (jar) implementor.implementJar(token, root);
            else implementor.implement(token, root);
        } catch (final InvalidPathException e) {
            System.err.println("couldn't convert path string to path: " + e.getMessage());
        } catch (final ClassNotFoundException e) {
            System.err.println("couldn't find the class by the provided name: " + e.getMessage());
        } catch (final ImplerException e) {
            System.err.println("implementation failed: " + e.getMessage());
        }
    }
}
