/*
 * An implementation of a min heap using java's ArrayList.
 */
package heap;

import java.util.ArrayList;
import java.lang.Comparable;
import java.lang.Exception;

public class Heap<T extends Comparable<T> > {

  private ArrayList<T> arr;

  public Heap(int capacity){
    super();
    arr = new ArrayList(capacity);
  }

  // Public Methods

  public void insert(T elem){
    arr.add(elem);
    propagateUp(arr.size() - 1);
  }

  public T peek() throws Exception {
    if (arr.size() > 0){
      return arr.get(0);
    } else {
      throw new Exception("Heap is empty");
    }
  }

  public T pop() throws Exception {
    T top = this.peek();
    swap(0, arr.size() - 1);
    arr.remove(arr.size() - 1);
    propagateDown(0);
    return top;
  }

  public void print() {
    System.out.println(arr.toString());
    System.out.println();
  }


  // Private Methods


  private void propagateDown(int i){
    int lChildIndex = getLChild(i);
    int rChildIndex = getRChild(i);
    T curr = arr.get(i);
    if (lChildIndex < arr.size()){
      T left = arr.get(lChildIndex);
      if (curr.compareTo(left) > 0){
        swap(i,lChildIndex);
        propagateDown(lChildIndex);
      }
    } else if (rChildIndex < arr.size()){
      T right = arr.get(rChildIndex);
      if (curr.compareTo(right) > 0){
        swap(i,rChildIndex);
        propagateDown(rChildIndex);
      }
    }
  }


  private void propagateUp(int i){
    if (i==0){
      return;
    } else {
      int parentIndex = getParent(i);
      T curr = arr.get(i);
      T paren = arr.get(parentIndex);
      if (curr.compareTo(paren) < 0){
        swap(i,parentIndex);
        propagateUp(parentIndex);
      }
    }
  }

  private void swap(int i, int j){
    T temp;
    temp = arr.get(i);
    arr.set(i,arr.get(j));
    arr.set(j, temp);
  }

  private static int getLChild(int i){
    return 2*i + 1;
  }

  private static int getRChild(int i){
    return 2*i + 2;
  }

  private static int getParent(int i){
    return (i-1) / 2;
  }

}


