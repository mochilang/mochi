public class Main {
    static Object[] res = _netLookupHost("www.kame.net");
    static Object addrs = res[0];
    static Object err = res[1];

    public static void main(String[] args) {
        if (err == null) {
            System.out.println(String.valueOf(addrs));
        } else {
            System.out.println(err);
        }
    }

    static Object[] _netLookupHost(String host) {
        try {
            java.net.InetAddress[] arr = java.net.InetAddress.getAllByName(host);
            String[] out = new String[arr.length];
            for (int i = 0; i < arr.length; i++) { out[i] = arr[i].getHostAddress(); }
            return new Object[]{out, null};
        } catch (Exception e) {
            return new Object[]{null, e.toString()};
        }
    }
}
