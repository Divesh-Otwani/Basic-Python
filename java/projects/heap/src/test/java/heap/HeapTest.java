/*
 * Testing the Heap class.
 */
package heap;

import org.junit.Test;
import static org.junit.Assert.*;

import java.util.Random;

import java.lang.Integer;

public class HeapTest {
  private Random r = new Random();
  private Heap<Integer> h = new Heap(20);

  @Test public void heapTest() {
    for (int i = 0; i < 16; ++i){
      h.insert(new Integer(r.nextInt(50)));
    }
    System.out.println("Original heap.");
    h.print();

    try{

      System.out.println("Adding -3");
      h.insert(new Integer(-3));
      h.print();

      System.out.println("Poping min.");
      assertEquals(h.pop(), new Integer(-3));
      h.print();

      System.out.println("Adding -2");
      h.insert(new Integer(-2));
      h.print();

      System.out.println("Adding 0");
      h.insert(new Integer(0));
      h.print();

      System.out.println("Poping min.");
      assertEquals(h.pop(), new Integer(-2));
      h.print();

      System.out.println("Poping min.");
      assertEquals(h.pop(), new Integer(0));
      h.print();

    } catch(Exception e) {
      return;
    }
  }


}
