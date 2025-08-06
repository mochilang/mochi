public class Main {
    static class MosaicResult {
        int[][] img;
        double[][] annos;
        String path;
        MosaicResult(int[][] img, double[][] annos, String path) {
            this.img = img;
            this.annos = annos;
            this.path = path;
        }
        MosaicResult() {}
        @Override public String toString() {
            return String.format("{'img': %s, 'annos': %s, 'path': '%s'}", String.valueOf(img), String.valueOf(annos), String.valueOf(path));
        }
    }


    static MosaicResult update_image_and_anno(String[] all_img_list, double[][][] all_annos, int[] idxs, int[] output_size, double[] scale_range, double filter_scale) {
        int height = output_size[0];
        int width = output_size[1];
        int[][] output_img = new int[0][];
        int r = 0;
        while (r < height) {
            int[] row = new int[0];
            int c = 0;
            while (c < width) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                c = c + 1;
            }
            output_img = ((int[][])(appendObj(output_img, row)));
            r = r + 1;
        }
        double scale_x = (scale_range[0] + scale_range[1]) / 2.0;
        double scale_y = (scale_range[0] + scale_range[1]) / 2.0;
        int divid_point_x = (((Number)((scale_x * (((Number)(width)).doubleValue())))).intValue());
        int divid_point_y = (((Number)((scale_y * (((Number)(height)).doubleValue())))).intValue());
        double[][] new_anno = new double[0][];
        String[] path_list = new String[0];
        int i = 0;
        while (i < idxs.length) {
            int index = idxs[i];
            String path = all_img_list[index];
            path_list = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path_list), java.util.stream.Stream.of(path)).toArray(String[]::new)));
            double[][] img_annos = ((double[][])(all_annos[index]));
            if (i == 0) {
                int y0 = 0;
                while (y0 < divid_point_y) {
                    int x0 = 0;
                    while (x0 < divid_point_x) {
output_img[y0][x0] = i + 1;
                        x0 = x0 + 1;
                    }
                    y0 = y0 + 1;
                }
                int j0 = 0;
                while (j0 < img_annos.length) {
                    double[] bbox = ((double[])(img_annos[j0]));
                    double xmin = bbox[1] * scale_x;
                    double ymin = bbox[2] * scale_y;
                    double xmax = bbox[3] * scale_x;
                    double ymax = bbox[4] * scale_y;
                    new_anno = ((double[][])(appendObj(new_anno, new double[]{bbox[0], xmin, ymin, xmax, ymax})));
                    j0 = j0 + 1;
                }
            } else             if (i == 1) {
                int y1 = 0;
                while (y1 < divid_point_y) {
                    int x1 = divid_point_x;
                    while (x1 < width) {
output_img[y1][x1] = i + 1;
                        x1 = x1 + 1;
                    }
                    y1 = y1 + 1;
                }
                int j1 = 0;
                while (j1 < img_annos.length) {
                    double[] bbox1 = ((double[])(img_annos[j1]));
                    double xmin1 = scale_x + bbox1[1] * (1.0 - scale_x);
                    double ymin1 = bbox1[2] * scale_y;
                    double xmax1 = scale_x + bbox1[3] * (1.0 - scale_x);
                    double ymax1 = bbox1[4] * scale_y;
                    new_anno = ((double[][])(appendObj(new_anno, new double[]{bbox1[0], xmin1, ymin1, xmax1, ymax1})));
                    j1 = j1 + 1;
                }
            } else             if (i == 2) {
                int y2 = divid_point_y;
                while (y2 < height) {
                    int x2 = 0;
                    while (x2 < divid_point_x) {
output_img[y2][x2] = i + 1;
                        x2 = x2 + 1;
                    }
                    y2 = y2 + 1;
                }
                int j2 = 0;
                while (j2 < img_annos.length) {
                    double[] bbox2 = ((double[])(img_annos[j2]));
                    double xmin2 = bbox2[1] * scale_x;
                    double ymin2 = scale_y + bbox2[2] * (1.0 - scale_y);
                    double xmax2 = bbox2[3] * scale_x;
                    double ymax2 = scale_y + bbox2[4] * (1.0 - scale_y);
                    new_anno = ((double[][])(appendObj(new_anno, new double[]{bbox2[0], xmin2, ymin2, xmax2, ymax2})));
                    j2 = j2 + 1;
                }
            } else {
                int y3 = divid_point_y;
                while (y3 < height) {
                    int x3 = divid_point_x;
                    while (x3 < width) {
output_img[y3][x3] = i + 1;
                        x3 = x3 + 1;
                    }
                    y3 = y3 + 1;
                }
                int j3 = 0;
                while (j3 < img_annos.length) {
                    double[] bbox3 = ((double[])(img_annos[j3]));
                    double xmin3 = scale_x + bbox3[1] * (1.0 - scale_x);
                    double ymin3 = scale_y + bbox3[2] * (1.0 - scale_y);
                    double xmax3 = scale_x + bbox3[3] * (1.0 - scale_x);
                    double ymax3 = scale_y + bbox3[4] * (1.0 - scale_y);
                    new_anno = ((double[][])(appendObj(new_anno, new double[]{bbox3[0], xmin3, ymin3, xmax3, ymax3})));
                    j3 = j3 + 1;
                }
            }
            i = i + 1;
        }
        if (filter_scale > 0.0) {
            double[][] filtered = new double[0][];
            int k = 0;
            while (k < new_anno.length) {
                double[] anno = ((double[])(new_anno[k]));
                double w = anno[3] - anno[1];
                double h = anno[4] - anno[2];
                if (filter_scale < w && filter_scale < h) {
                    filtered = ((double[][])(appendObj(filtered, anno)));
                }
                k = k + 1;
            }
            new_anno = ((double[][])(filtered));
        }
        return new MosaicResult(output_img, new_anno, path_list[0]);
    }

    static void main() {
        String[] all_img_list = ((String[])(new String[]{"img0.jpg", "img1.jpg", "img2.jpg", "img3.jpg"}));
        double[][][] all_annos = ((double[][][])(new double[][][]{new double[][]{new double[]{0.0, 0.1, 0.1, 0.4, 0.4}}, new double[][]{new double[]{1.0, 0.2, 0.3, 0.5, 0.7}}, new double[][]{new double[]{2.0, 0.6, 0.2, 0.9, 0.5}}, new double[][]{new double[]{3.0, 0.5, 0.5, 0.8, 0.8}}}));
        int[] idxs = ((int[])(new int[]{0, 1, 2, 3}));
        int[] output_size = ((int[])(new int[]{100, 100}));
        double[] scale_range = ((double[])(new double[]{0.4, 0.6}));
        double filter_scale = 0.05;
        MosaicResult res = update_image_and_anno(((String[])(all_img_list)), ((double[][][])(all_annos)), ((int[])(idxs)), ((int[])(output_size)), ((double[])(scale_range)), filter_scale);
        double[][] new_annos = ((double[][])(res.annos));
        String path_1 = res.path;
        System.out.println("Base image: " + path_1);
        System.out.println("Mosaic annotation count: " + _p(new_annos.length));
        int i_1 = 0;
        while (i_1 < new_annos.length) {
            double[] a = ((double[])(new_annos[i_1]));
            System.out.println(_p(_geto(a, 0)) + " " + _p(_geto(a, 1)) + " " + _p(_geto(a, 2)) + " " + _p(_geto(a, 3)) + " " + _p(_geto(a, 4)));
            i_1 = i_1 + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
