public class Good1 {
    public static void main(String[] args) {
        int i = 0;
        while (i < 10) {
            System.out.println(printIt(i));
            i++;
        }
    }

    public static String printIt(int i) {
        String print = "Printing: ";

        return print + i;
    }
}