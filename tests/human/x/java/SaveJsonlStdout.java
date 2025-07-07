public class SaveJsonlStdout {
    static class Person {String name; int age; Person(String n,int a){name=n;age=a;}}
    public static void main(String[] args) {
        Person[] people = { new Person("Alice",30), new Person("Bob",25) };
        for(Person p: people){
            System.out.println("{\"name\":\""+p.name+"\",\"age\":"+p.age+"}");
        }
    }
}
