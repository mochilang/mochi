import java.util.*;

class PersonUS {
    String name; int age; String status;
    PersonUS(String name,int age,String status){this.name=name;this.age=age;this.status=status;}
}

public class UpdateStmt {
    public static void main(String[] args) {
        List<PersonUS> people = new ArrayList<>(Arrays.asList(
            new PersonUS("Alice",17,"minor"),
            new PersonUS("Bob",25,"unknown"),
            new PersonUS("Charlie",18,"unknown"),
            new PersonUS("Diana",16,"minor")
        ));

        for(PersonUS p: people){
            if(p.age >= 18){
                p.status = "adult";
                p.age = p.age + 1;
            }
        }

        boolean ok =
            people.get(0).age==17 && people.get(0).status.equals("minor") &&
            people.get(1).age==26 && people.get(1).status.equals("adult") &&
            people.get(2).age==19 && people.get(2).status.equals("adult") &&
            people.get(3).age==16 && people.get(3).status.equals("minor");

        if(!ok) throw new AssertionError("update failed");
        System.out.println("ok");
    }
}
