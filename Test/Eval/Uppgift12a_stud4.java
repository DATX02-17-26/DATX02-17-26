public class Uppgift12a_stud4 {
    public static void main(String[] args) {
        double pi = 0;
        int i = 0;
        for (double denominator = 1; i < 500; denominator = denominator + 2) {

            if (i % 2 == 0) {
                pi = pi + (1 / denominator);
            } else {
                pi = pi - (1 / denominator);
            }
            i++;
        }
        pi = pi * 4;
        System.out.println(pi);
    }
}
