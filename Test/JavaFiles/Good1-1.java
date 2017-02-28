public class Class1 {
    public static void method1(String[] val1) {
        int val2 = 0;
        while (val2 < 10) {
            System.out.println(printIt(val2));
            val2++;
        }
    }

    public static String printIt(int val3) {
        String val4 = "Printing: ";

        return val4 + val3;
    }
}