public class Main {
    static int seed = 0;
    static class Character {
        String title;
        String description;
        String image_file;
        Character(String title, String description, String image_file) {
            this.title = title;
            this.description = description;
            this.image_file = image_file;
        }
        Character() {}
        @Override public String toString() {
            return String.format("{'title': '%s', 'description': '%s', 'image_file': '%s'}", String.valueOf(title), String.valueOf(description), String.valueOf(image_file));
        }
    }

    static Character[] characters;
    static Character c;

    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static int random_int(int a, int b) {
        return a + (Math.floorMod(rand(), (b - a)));
    }

    static void save_image(String _name) {
    }

    static Character random_anime_character() {
        int idx = random_int(0, characters.length);
        Character ch = characters[idx];
        save_image(ch.image_file);
        return ch;
    }
    public static void main(String[] args) {
        seed = 123456789;
        characters = ((Character[])(new Character[]{new Character("Naruto Uzumaki", "A spirited ninja of the Hidden Leaf Village.", "naruto.png"), new Character("Sailor Moon", "A magical girl who fights for love and justice.", "sailor_moon.png"), new Character("Spike Spiegel", "A bounty hunter with a laid-back attitude.", "spike_spiegel.png")}));
        c = random_anime_character();
        System.out.println(c.title);
        System.out.println("");
        System.out.println(c.description);
        System.out.println("");
        System.out.println("Image saved : " + c.image_file);
    }
}
