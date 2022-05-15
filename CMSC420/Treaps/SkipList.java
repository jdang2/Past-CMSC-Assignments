package cmsc420_f21;

import java.util.Random;
import java.util.TreeMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

//-------------------------------------------------------------------------
//Add your code here. You may import whatever (standard) packages you like.
//You may also create additional files, if you like, but be sure to upload
//them as well for grading.
//-------------------------------------------------------------------------

public class SkipList<Key extends Comparable<Key>> {
	// -------------------------------------------------------------------------
	// Here is the utility functions.
	// You should NOT change this. You also should NOT use any of the functions
	// in your own impelementation.
	// -------------------------------------------------------------------------
	
	private Random generator = new Random();
	
	public void setRandomSeed(int seed) {
		generator.setSeed(seed);
	}
	private int getRandomLevel(double p) {
		int level = 0;
		while (generator.nextDouble() <= p) {
			level += 1;
		}
		return level;
	}
	public void clear() {
		head = null;
	}
	
	// -------------------------------------------------------------------------
	// Here's the provided interface for the node class. 
	// You should NOT delete anything here, but you can add any 
	// additional methods and/or data members that you like. 
	// -------------------------------------------------------------------------
	
	private class Node {
		//ArrayList<Node> next;
		Key key;
		int level;
		boolean is_head = false;
		Node next;
		Node down;
		
		
		Node(Key k, int level, Node next, Node down) {
			this.key = k;
//			next = new ArrayList<Node>();
//			for (int i = 0; i <= level; i++) 
//				next.add(null);
			this.level = level;
			this.next = next;
			this.down = down;
			
		}
		
		public boolean lessThan(Key key) {
			return(next != null && next.key.compareTo(key) < 0);
		}
		
	      public boolean equalTo(Key key) {
	            return (next != null && next.key.equals(key));
	        }
	}
	// -------------------------------------------------------------------------
	// Here is the public interface. You should not change the function signature.
	// But you can add any additional methods and/or data members that you like.
	// -------------------------------------------------------------------------
	private Node head;
	
	
	// Constructor
	public SkipList() {
		head = new Node(null, 0, null, null);
	}
	// Insert key k to the skip list. 
	// Invoke function getRandomLevel with input p to get the level of 
	// the newly inserted node.
	public void insert(Key k, double p) {
		int level = getRandomLevel(p);
		Node save = null;
		int counter = level;

		if(head == null) {
			head = new Node(null,0, null, null);
		}
		
		
		if(level > head.level) {
			save = new Node(null, level, null, null);
			Node temp = head;
			temp = save;
			Node currentHead = head;
			while(temp != null && counter != 0) {
				Node n = new Node(null, counter-1, null, null);
				
				if(temp.down == null || temp.down.next == null) {
					if(head.level == temp.level-1) {
						temp.down = currentHead;
					}else {
						temp.down = n;
					}
					temp = temp.down;
				}else {
					head = temp;
				}
				counter--;
				 
			}
			head = save;

			
		}
		
		Node curr = head;
		Node prevLevel = null;

		
		while(curr != null) {
			while(curr.lessThan(k)) {
				curr = curr.next;
			}
			
			if(level < curr.level) {
				curr = curr.down;
				continue;
			}
			
			Node newNode = new Node(k, curr.level, curr.next, null);
			
			if(prevLevel != null) {
				prevLevel.down = newNode;
			}
			
			prevLevel = newNode;
			curr.next = newNode;
			curr = curr.down;
 
		}
	}
	// Find key k in the tree and return number of comparisons needed.
	// Return -1 if key is not found.
	public int find(Key k) {
		Node cur = head;
		boolean found = false;
		int counter = 0;
		
        while (cur != null) {
        	
             if(cur.lessThan(k)) {
            	 if(cur.key != null && cur.next != null) {
            		 counter++;
            	 }
                cur = cur.next;
                continue;
            }

            if (cur.equalTo(k)) {
            	if(cur.key != null) {
            		counter++;
                	counter++;
            	}else {
            		counter++;
            	}
            	
                found = true;
                cur = null;
                continue;
            }
            
            if(cur.next != null) {
       		 counter++;
       	 	}
            cur = cur.down;
        }
        
        if(found) {
        	return counter;
        }else {
        	return -1;
        }
		
	}
	// Print your skiplist content.
	// The format is <node1.key>,<node1.level>|<node2.key>,<node2.level>|....|<final_node.key>,<final_node.level>
	public String print() {
		Map<Key, Integer> toSort = new TreeMap<Key, Integer>();
		List<Node> levels = new ArrayList<Node>();
		
		Node temp = head;
		while(temp != null) {
			if(temp.key == null) {
				levels.add(temp);
				temp = temp.down;
			}
		}
		
				
		
		for(int i = 0; i < levels.size(); i++) {
			Node currentLevel = levels.get(i);
			while(currentLevel != null) {
				if(currentLevel.key == null) {
					currentLevel = currentLevel.next;
					continue;
				}else {
					
					if(toSort.containsKey(currentLevel.key)) {
						if(toSort.get(currentLevel.key) < currentLevel.level) {
							toSort.put(currentLevel.key, currentLevel.level);
						}
					}
					if(!toSort.containsKey(currentLevel.key)) {
						toSort.put(currentLevel.key, currentLevel.level);
					}
					 
				}
				currentLevel = currentLevel.next;
			}
		}
		
 		StringBuffer str = new StringBuffer("");
 		for(Entry<Key, Integer> entry : toSort.entrySet()) {
 			Key key = entry.getKey();
 			Integer value = entry.getValue();
 			
 			str.append(String.valueOf(key) + "," + String.valueOf(value) + "|");
 		}
		return str.toString();
	}
}
