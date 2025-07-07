import java.io.*;
import java.util.*;

class PersonLY {String name; int age; String email;}

public class LoadYaml {
    public static void main(String[] args) throws Exception {
        File path = new File("../../interpreter/valid/people.yaml");
        List<PersonLY> people = new ArrayList<>();
        try(BufferedReader br = new BufferedReader(new FileReader(path))){
            PersonLY p=null;
            String line;
            while((line=br.readLine())!=null){
                line=line.trim();
                if(line.startsWith("- name:")){
                    if(p!=null) people.add(p);
                    p=new PersonLY();
                    p.name=line.substring(line.indexOf(':')+1).trim();
                } else if(line.startsWith("age:")) {
                    if(p!=null) p.age=Integer.parseInt(line.substring(line.indexOf(':')+1).trim());
                } else if(line.startsWith("email:")) {
                    if(p!=null) p.email=line.substring(line.indexOf(':')+1).trim();
                }
            }
            if(p!=null) people.add(p);
        }
        for(PersonLY p:people){
            if(p.age>=18) System.out.println(p.name+" "+p.email);
        }
    }
}
