// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// case-sensitivity-of-identifiers.mochi
import java.util.*;

public class CaseSensitivityOfIdentifiers {
    static void main() {
        String pkg_dog = "Salt";
        String Dog = "Pepper";
        String pkg_DOG = "Mustard";
        Object packageSees = (d1, d2, d3) -> {
            System.out.println("Package sees: " + d1 + " " + d2 + " " + d3);
            return new PkgDogDogPkgDOG(true, true, true);
        };
        Object d = packageSees(pkg_dog, Dog, pkg_DOG);
        System.out.println("There are " + String.valueOf(d.size()) + " dogs.\n");
        String dog = "Benjamin";
        d = packageSees(pkg_dog, Dog, pkg_DOG);
        System.out.println("Main sees:   " + dog + " " + Dog + " " + pkg_DOG);
        ((Map<?,?>)d).put("dog", true);
        ((Map<?,?>)d).put("Dog", true);
        ((Map<?,?>)d).put("pkg_DOG", true);
        System.out.println("There are " + String.valueOf(d.size()) + " dogs.\n");
        Dog = "Samba";
        d = packageSees(pkg_dog, Dog, pkg_DOG);
        System.out.println("Main sees:   " + dog + " " + Dog + " " + pkg_DOG);
        ((Map<?,?>)d).put("dog", true);
        ((Map<?,?>)d).put("Dog", true);
        ((Map<?,?>)d).put("pkg_DOG", true);
        System.out.println("There are " + String.valueOf(d.size()) + " dogs.\n");
        String DOG = "Bernie";
        d = packageSees(pkg_dog, Dog, pkg_DOG);
        System.out.println("Main sees:   " + dog + " " + Dog + " " + DOG);
        ((Map<?,?>)d).put("dog", true);
        ((Map<?,?>)d).put("Dog", true);
        ((Map<?,?>)d).put("pkg_DOG", true);
        ((Map<?,?>)d).put("DOG", true);
        System.out.println("There are " + String.valueOf(d.size()) + " dogs.");
    }
    public static void main(String[] args) {
        main();
    }
}
