import java.io.Serializable;
import java.lang.Cloneable;

public class Test implements Serializable, Cloneable {
    public static int x = 2;
    public static void main(String[] args) {
	System.out.println("Hello world: " + x);
    }
}

