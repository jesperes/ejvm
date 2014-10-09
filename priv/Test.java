import java.io.Serializable;
import java.lang.Cloneable;

public class Test implements Serializable, Cloneable {
  public static int x = 2;
  public static void main(String[] args) {
    for (int i = 0; i < 10; i++) {
      System.out.println("Hello world: " + x);
    }
  }
}

