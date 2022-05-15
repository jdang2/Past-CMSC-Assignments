package cmsc420_f21;
import java.util.Random;

//-------------------------------------------------------------------------
//Add your code here. You may import whatever (standard) packages you like.
//You may also create additional files, if you like, but be sure to upload
//them as well for grading.
//-------------------------------------------------------------------------

public class Treap<Key extends Comparable<Key>> {
	
	// -------------------------------------------------------------------------
	// Here is the utility functions.
	// You should NOT change this. You also should NOT use any of the functions
	// in your own implementation.
	// -------------------------------------------------------------------------
	
	private Random generator = new Random();
	private int rng_upperbound = 0;
	
	public void setRandomSeed(int seed) {
		generator = new Random(seed);
	}
	public void setRngUpperbound(int u) {
		rng_upperbound = u;
	}
	private int getNextPriority() {
		return generator.nextInt(rng_upperbound);
	}
	public void clear() {
		root = null;
	}
	
	// -------------------------------------------------------------------------
	// Here's the provided interface for the node class. 
	// You should NOT delete anything here, but you can add any 
	// additional methods and/or data members that you like. 
	// -------------------------------------------------------------------------
	
	private class Node {
		Node left, right;
		Key key;
		int priority;
		Node(Key k) {
			this.left = null;
			this.right = null;
			this.key = k;
			this.priority = getNextPriority();
		}
	}
	
	private Node root;
	
	// -------------------------------------------------------------------------
	// Here are functions for sanity check of your treap. Do NOT modify this. 
	// -------------------------------------------------------------------------
	private boolean keyCheckHelper(Node root) {
		if (root == null)
			return true;
		boolean check_left_tree = keyCheckHelper(root.left);
		boolean check_right_tree = keyCheckHelper(root.right);
		boolean check = true;
		if (root.left != null)
			check = check && (root.left.key.compareTo(root.key) <= 0);
		if (root.right != null)
			check = check && (root.right.key.compareTo(root.key) > 0);
		return check && check_left_tree && check_right_tree;
	}
	
	
	private boolean priorityCheckHelper(Node root) {
		if (root == null)
			return true;
		boolean check_left_tree = keyCheckHelper(root.left);
		boolean check_right_tree = keyCheckHelper(root.right);
		boolean check = true;
		if (root.left != null)
			check = check && (root.left.priority >= root.priority);
		if (root.right != null)
			check = check && (root.right.priority >= root.priority);
		return check && check_left_tree && check_right_tree;
	}
	
	
	
	public boolean isTreap() {
		if (root == null)
			return false;
		boolean keyCheck = keyCheckHelper(root);
		boolean priorityCheck = priorityCheckHelper(root);
		return keyCheck && priorityCheck;
	}
	
	
	// -------------------------------------------------------------------------
	// Here is the public interface. You should fill in the implementation.
	// You can also add any additional methods and/or data members that you like.
	// -------------------------------------------------------------------------
	
	// Constructor
	public Treap() {
		root = null;
	} 
	
	
	public Node rotateLeft(Node root) {
		Node r = root.right;
		Node temp = root.right.left;
		
		r.left = root;
		root.right = temp;
		
		return r;
	}
	
	public Node rotateRight(Node root) {
		Node l = root.left;
		Node temp = root.left.right;
		
		l.right = root;
		root.left = temp;
		
		return l;
	}
	
	// Insert key k to the tree.
	public void insert(Key k) {
		root = insertAux(root, k);
	}
	
	public Node insertAux(Node root, Key k) {
		if(root == null) {
			return new Node(k);
		}
		
		if(k.compareTo(root.key) < 0) {
			root.left = insertAux(root.left, k);
			
			if(root.left != null) {
				int x = root.priority;
				int y = root.left.priority;
				if((x <= y) == false) {
					root = rotateRight(root);
				}
			}
		}else {
			root.right = insertAux(root.right, k);
			
			if(root.right != null) {
				int x = root.priority;
				int z = root.right.priority;
				if((x <= z) == false) {
					root = rotateLeft(root);
				}
			}
		}
		
		return root;
	} 
	// Find key k in the tree and return number of comparisons needed.
	// Return -1 if key is not found.
	public int find(Key k)  {
		if(counter != 0) {
			counter = 0;
		}
		
		if(found != false) {
			found = false;
		}
		return findAux(root, k);
	}
	
	private int counter = 0;
	private boolean found = false;
	
	public int findAux(Node root, Key k) {
		if(root == null) {
			return -1;
		}
		
		if(root.key.equals(k)) {
			counter++;
			found = true;
			return counter;
		}else {
			counter++;
		}
		
		if(k.compareTo(root.key) < 0) {
			return findAux(root.left, k);
		}
	
		findAux(root.right, k);
		
		if(found) {
			return counter;
		}else {
			return -1;
		}
		
	}
	

}
