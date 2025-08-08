public class Main {
    static String base_url;
    static String sample_url;
    static String link;

    static String download_video(String url) {
        String request_url = base_url + url;
        return request_url;
    }
    public static void main(String[] args) {
        base_url = "https://downloadgram.net/wp-json/wppress/video-downloader/video?url=";
        sample_url = "https://www.instagram.com/p/Example/";
        link = String.valueOf(download_video(sample_url));
        System.out.println("Download link:" + " " + link);
    }
}
