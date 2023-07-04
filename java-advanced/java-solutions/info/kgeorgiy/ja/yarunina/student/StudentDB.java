package info.kgeorgiy.ja.yarunina.student;

import info.kgeorgiy.java.advanced.student.*;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@SuppressWarnings("unused")
public class StudentDB implements GroupQuery {
    private static final Comparator<Student> STUDENT_NAMES_COMPARATOR =
            Comparator.comparing(Student::getLastName, Comparator.reverseOrder())
                    .thenComparing(Student::getFirstName, Comparator.reverseOrder())
                    .thenComparing(Student::getId);

    private <T> List<T> getStudentsAttributes(final List<Student> students,
                                              final Function<Student, T> function) {
        return students.stream()
                .map(function)
                .collect(Collectors.toList());
    }

    @Override
    public List<String> getFirstNames(final List<Student> students) {
        return getStudentsAttributes(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(final List<Student> students) {
        return getStudentsAttributes(students, Student::getLastName);
    }

    @Override
    public List<String> getFullNames(final List<Student> students) {
        return getStudentsAttributes(students,
                x -> String.format("%s %s", x.getFirstName(), x.getLastName()));
    }

    // :NOTE: лишний промежуточный лист fixed
    @Override
    public Set<String> getDistinctFirstNames(final List<Student> students) {
        return students.stream()
                .map(Student::getFirstName)
                .collect(Collectors.toCollection(TreeSet::new));
    }

    @Override
    public List<GroupName> getGroups(final List<Student> students) {
        return getStudentsAttributes(students, Student::getGroup);
    }

    @Override
    public String getMaxStudentFirstName(final List<Student> students) {
        return students.stream()
                .max(Comparator.comparing(Student::getId))
                .map(Student::getFirstName).orElse("");
    }

    private List<Student> sortStudents(final Collection<Student> students,
                                       final Comparator<Student> comparator) {
        return students.stream()
                .sorted(comparator)
                .collect(Collectors.toList());
    }

    @Override
    public List<Student> sortStudentsById(final Collection<Student> students) {
        return sortStudents(students, Comparator.comparing(Student::getId));
    }

    @Override
    public List<Student> sortStudentsByName(final Collection<Student> students) {
        return sortStudents(students, STUDENT_NAMES_COMPARATOR);
    }

    private Stream<Map.Entry<GroupName, List<Student>>> getGroupStream(final Collection<Student> students) {
        return students.stream()
                .collect(Collectors.groupingBy(Student::getGroup))
                .entrySet()
                .stream();
    }

    private List<Group> getGroupsSorted(final Collection<Student> students,
                                        final Comparator<Student> comparator) {
        return getGroupStream(students)
                .map(x -> new Group(x.getKey(), sortStudents(x.getValue(), comparator)))
                .sorted(Comparator.comparing(Group::getName))
                .collect(Collectors.toList());
    }

    @Override
    public List<Group> getGroupsByName(final Collection<Student> students) {
        return getGroupsSorted(students, STUDENT_NAMES_COMPARATOR);
    }

    @Override
    public List<Group> getGroupsById(final Collection<Student> students) {
        return getGroupsSorted(students, Comparator.comparing(Student::getId));
    }

    private GroupName getLargestGroup(final Collection<Student> students,
                                      final Comparator<Map.Entry<GroupName, List<Student>>> comparator) {
        return getGroupStream(students)
                .max(comparator)
                .map(Map.Entry::getKey)
                .orElse(null);
    }

    @Override
    public GroupName getLargestGroup(final Collection<Student> students) {
        return getLargestGroup(students,
                Comparator.comparingInt((Map.Entry<GroupName, List<Student>> entry) -> entry.getValue().size())
                        .thenComparing(Map.Entry.comparingByKey()));
    }

    @Override
    public GroupName getLargestGroupFirstName(final Collection<Student> students) {
        return getLargestGroup(students,
                Comparator.comparingInt((Map.Entry<GroupName, List<Student>> entry)
                                -> getDistinctFirstNames(entry.getValue()).size())
                        .thenComparing(Map.Entry.comparingByKey(Comparator.reverseOrder())));
    }


    private <T> Stream<Student> filterStudents(final Collection<Student> students,
                                               final Function<Student, T> function,
                                               final T match) {
        return students.stream()
                .filter(x -> Objects.equals(function.apply(x), match));
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(final Collection<Student> students,
                                                       final GroupName group) {
        return filterStudents(students, Student::getGroup, group)
                .collect(Collectors.toMap(Student::getLastName,
                        Student::getFirstName, BinaryOperator.minBy(Comparator.naturalOrder())));
    }

    private <T> List<Student> findStudents(final Collection<Student> students,
                                           final Function<Student, T> function,
                                           final T match) {
        return filterStudents(students, function, match)
                .sorted(STUDENT_NAMES_COMPARATOR)
                .collect(Collectors.toList());
    }

    @Override
    public List<Student> findStudentsByFirstName(final Collection<Student> students,
                                                 final String name) {
        return findStudents(students, Student::getFirstName, name);
    }

    @Override
    public List<Student> findStudentsByLastName(final Collection<Student> students,
                                                final String name) {
        return findStudents(students, Student::getLastName, name);
    }

    @Override
    public List<Student> findStudentsByGroup(final Collection<Student> students,
                                             final GroupName group) {
        return findStudents(students, Student::getGroup, group);
    }
}
