public class Uppgift12a_stud3 {
    public static void main(String[] args) {
        System.out.println(countPi());
    }

    public double countPi() {

        double pi = 0;
        double denominator = 1;

        //Use 400 and not 500
        for (int i = 0, denominator; i < 400; i++) {
            //Introdueced fault != and not ==
            if (i % 2 != 0) {
                pi = pi + (1 / denominator);
            } else {
                pi = pi - (1 / denominator);
            }
            denominator = denominator + 2;
        }
        return pi * 4;
    }
}
