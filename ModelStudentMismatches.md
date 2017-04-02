# A summary of mismatches between model and student solutions

## Auxiliary variables
The model solution `Uppgift12a_2.java`:
```Java
public class Uppgift12a_2
{
  public static void main (String[] args)
  {
    double pi = 0;
    double num = 1;
    int den = 1;
    for (int i = 0 ; i < 500 ; i++)
    {
      pi += num / den;
      num *= -1;
      den += 2;
    }
    System.out.println((pi * 4));
  }
}
```
Does not match the student solution
```Java
public class Uppgift12
{
  public static void main (String[] args)
  {
    double sum = 0;
    double num = 1;
    double div = 1;
    int den = 1;
    for (int round = 0 ; round <= 500 ; round++)
    {
      div = num / den;
      sum += div;
      num *= -1;
      den += 2;
    }
    System.out.println((sum * 4));
  }
}
```
Because an auxiliary variable `div` is used in the student solution.

Student solution:
```Java
public class Uppgift12
{
  public static void main (String[] arg)
  {
    double den = 1;
    double num = 1;
    double sum = 0;
    for (int i = 0 ; i <= 499 ; i++)
    {
      sum = sum + num / den;
      num = num * -1;
      den = den + 2;
    }
    double pi = sum * 4;
    System.out.println(pi);
  }
}
```
Doesn't match the model solution because of the auxiliary variable `pi`.

## Output method
The model solution:
```Java
public class Uppgift12a_1 {
   public static void main(String[] args) {

      double pi = 0;
      double denominator = 1;

      for (int i = 0; i < 500; i++) {

         if (i % 2 == 0) {
            pi = pi + (1 / denominator);
         } else {
            pi = pi - (1 / denominator);
         }
         denominator = denominator + 2;
      }
      pi = pi * 4;
      System.out.println(pi);
   }
 }
```
Doesn't match the student solution:
```Java
import javax.swing.*;
public class Uppgift12
{
  public static void main (String[] arg)
  {
    int denominator = 1;
    double sum = 1;
    for (int i = 1 ; i <= 500 ; i = i + 1)
    {
      denominator = denominator + 2;
      if (i % 2 == 0)
        sum = sum + 1.0 / denominator;
      else
        sum = sum - 1.0 / denominator;
    }
    JOptionPane.showMessageDialog(null, (sum * 4));
  }
}
```
Because `JOptionPane.showMessageDialog(null, (sum * 4))` is used as output method
instead of `System.out.println(pi)`. It also does not match because of the indexing method.
(It may also be incorrect).

## Variable Ordering + inlining
The model solution:
```Java
public class Uppgift12a_1 {
   public static void main(String[] args) {

      double pi = 0;
      double denominator = 1;

      for (int i = 0; i < 500; i++) {

         if (i % 2 == 0) {
            pi = pi + (1 / denominator);
         } else {
            pi = pi - (1 / denominator);
         }
         denominator = denominator + 2;
      }
      pi = pi * 4;
      System.out.println(pi);
   }
 }
```
The student solution:
```Java
public class Uppgift12
{
  public static void main (String[] arg)
  {
    double var1 = 1;
    double pi = 0;
    for (int i = 0 ; i < 500 ; i = i + 1)
    {
      if (i % 2 == 0)
      {
        pi = pi + 1 / var1;
      }
      else
      {
        pi = pi - 1 / var1;
      }
      var1 = var1 + 2;
    }
    System.out.println((pi * 4));
  }
}
```
The problem here is the ordering between `var1` and `pi` in the student solution.
This should be caught by the strategies when that feature is complete.
There is one more problem though, the difference between `pi = pi * 4; System.out.println(pi);`
and `System.out.println((pi*4));`.
The final problem is the difference between `i = i + 1` and `i++` in the loop header.
