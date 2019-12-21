/*
 * A Tiny Binary Tree -- for practice
 */
package bstree;

public class BSTree<T extends Comparable> {

  protected T value;
  protected BSTree<T> left;
  protected BSTree<T> right;
  private String prefix = "";

  public BSTree(T val){
    super();
    this.value = val;
  }

  public boolean insert(T elem){
    int comparison = elem.compareTo(this.value);
    BSTree<T> newTree = new BSTree(elem);
    if (comparison < 0){
      if (this.left == null){
        this.left = newTree;
        return true;
      } else {
        return this.left.insert(elem);
      }
    } else if (comparison > 0) {
        if (this.right == null){
          this.right = newTree;
          return true;
        } else {
          return this.right.insert(elem);
        }
    } else {
        return false;
    }
  }

  public boolean find(T elem){
    int comparison = elem.compareTo(value);
    if (comparison == 0){
      return true;
    } else if (comparison < 0 && left != null){
        return left.find(elem);
    } else if (right != null){
        return right.find(elem);
    } else {
      return false;
    }
  }

  public void print(){
    this.printPrefix("");
    System.out.println("\n");
   }

  private void printPrefix(String prefix){
    System.out.print(prefix);
    System.out.print(this.value.toString() + "\n");
    String newPrefix = prefix + " ";
    if (this.left != null){
      System.out.print(prefix + "L: ");
      left.printPrefix(newPrefix);
    }
    if (this.right != null){
      System.out.print(prefix + "R: ");
      right.printPrefix(newPrefix);
    }
  }

}
