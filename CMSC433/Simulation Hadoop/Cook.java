package cmsc433;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

/**
 * Cooks are simulation actors that have at least one field, a name.
 * When running, a cook attempts to retrieve outstanding orders placed
 * by Customer and process them.
 */
public class Cook implements Runnable {
	private final String name;
	
	public HashMap<Food, Machines> machine; //HashMap of relevant Food/Machine, Key: Food Type, Value: Corresponding Machine
	public HashMap<Integer, HashMap<Food, LinkedList<Thread>>> thread; //HashMap of thread specific to order numbers, Key: OrderNum, Value: Food of order + thread
	/**
	 * You can feel free modify this constructor. It must
	 * take at least the name, but may take other parameters
	 * if you would find adding them useful.
	 *
	 * @param: the name of the cook
	 */
	public Cook(String name, HashMap<Food, Machines> machine) {
		this.name = name;
		this.machine = machine;
		this.thread = new HashMap<Integer, HashMap<Food, LinkedList<Thread>>>();
	}

	public String toString() {
		return name;
	}
	
	
	/**
	 * This method executes as follows. The cook tries to retrieve
	 * orders placed by Customers. For each order, a List<Food>, the
	 * cook submits each Food item in the List to an appropriate
	 * Machine type, by calling makeFood(). Once all machines have
	 * produced the desired Food, the order is complete, and the Customer
	 * is notified. The cook can then go to process the next order.
	 * If during its execution the cook is interrupted (i.e., some
	 * other thread calls the interrupt() method on it, which could
	 * raise InterruptedException if the cook is blocking), then it
	 * terminates.
	 */
	public void run() {
		
		//Cook reports to work
		Simulation.logEvent(SimulationEvent.cookStarting(this));
		try {
			while (true) {
				List<Food> order = null;
				int orderNum;
				
				synchronized(Simulation.newOrder) {
					//Waits for orders from the restaurant
					while(!Simulation.orderOpen()) {
						Simulation.newOrder.wait();
					}
					//Once order available, get orderNum 
					orderNum = Simulation.getNextOrderNum();
				}
				
				//Get list of food associated with orderNum
				order = Simulation.getOrder(orderNum);
				//Start cooking process given an actual order and the orderNum
				cookOrder(order, orderNum);
			}
		} catch (InterruptedException e) {
			// This code assumes the provided code in the Simulation class
			// that interrupts each cook thread when all customers are done.
			// You might need to change this if you change how things are
			// done in the Simulation class.
			Simulation.logEvent(SimulationEvent.cookEnding(this));
		}
	}
	
	public void cookOrder(List<Food> order, int orderNum) throws InterruptedException{
		//Lock thread specific to the orderNum
		synchronized(Simulation.getLock(orderNum)) {
			//Cook Receives Order
			Simulation.logEvent(SimulationEvent.cookReceivedOrder(this, order, orderNum));
			thread.put(orderNum, new HashMap<Food, LinkedList<Thread>>());
			//
			Food[] foods = { FoodType.fries, FoodType.pizza, FoodType.subs, FoodType.soda };
			int[] count = new int[4];
			//Go through given order, count how many of each food is present
			for(Food food: order) {
				if(food == FoodType.fries) {
					count[0] += 1;
				}else if(food == FoodType.pizza) {
					count[1] += 1;
				}else if(food == FoodType.subs) {
					count[2] += 1;
				}else if(food == FoodType.soda) {
					count[3] += 1;
				}else {
					throw new UnsupportedOperationException("Invalid food option");
				}
			}
			
			//Start from Fries -> Pizza -> Subs -> Soda in order to cook the appropriate amount needed
			for(int i = 0; i < 4; i++) {
				Food food = foods[i];
				thread.get(orderNum).put(food, new LinkedList<Thread>());
				
				for(int j = 0; j < count[i]; j++) {
					//Get the appropriate machine for the food type
					Machines machines = machine.get(food);
					//Cook Started Food
					Simulation.logEvent(SimulationEvent.cookStartedFood(this, food, orderNum));
					Simulation.startOrder(this, orderNum);
					//Corresponding machine makes food within their own threads
					Thread t = machines.makeFood(food);
					thread.get(orderNum).get(food).add(t);
				}
			}
			
			//Wait for each food type to finish
			for(Food food : thread.get(orderNum).keySet()) {
				for(Thread t: thread.get(orderNum).get(food)) {
					t.join();
					//Cook finishes specified food 
					Simulation.logEvent(SimulationEvent.cookFinishedFood(this, food, orderNum));
				}
			}
			//Cook completes order
			Simulation.logEvent(SimulationEvent.cookCompletedOrder(this, orderNum));
			Simulation.completeOrder(this, orderNum);
		}
	}
}
