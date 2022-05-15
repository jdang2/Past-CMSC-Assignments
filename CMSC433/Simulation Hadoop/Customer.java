package cmsc433;

import java.util.List;

/**
 * Customers are simulation actors that have two fields: a name, and a list
 * of Food items that constitute the Customer's order. When running, an
 * customer attempts to enter the Ratsie's (only successful if the
 * Ratsie's has a free table), place its order, and then leave the
 * Ratsie's when the order is complete.
 */
public class Customer implements Runnable {
	// JUST ONE SET OF IDEAS ON HOW TO SET THINGS UP...
	private final String name;
	private final List<Food> order;
	private final int orderNum;

	private static int orderCounter = 0;
	private static Object lock = new Object();
	
	/**
	 * You can feel free modify this constructor. It must take at
	 * least the name and order but may take other parameters if you
	 * would find adding them useful.
	 */
	public Customer(String name, List<Food> order) {
		this.name = name;
		this.order = order;
		
		synchronized(lock) {
			//Makes sure number of orders is kept tracked
			this.orderNum = ++orderCounter;
		}
	}

	public String toString() {
		return name;
	}

	/**
	 * This method defines what an Customer does: The customer attempts to
	 * enter the Ratsie's (only successful when the Ratsie's has a
	 * free table), place its order, and then leave the Ratsie's
	 * when the order is complete.
	 */
	public void run() {
		//Customer going to Ratsies  
		Simulation.logEvent(SimulationEvent.customerStarting(this));
		//Customer enters Ratsies
		Simulation.enteredRatsies(this);
		Simulation.logEvent(SimulationEvent.customerEnteredRatsies(this));
		 
		//Customer submits order
		Simulation.logEvent(SimulationEvent.customerPlacedOrder(this, order, orderNum));
		Simulation.submitOrder(this, order, orderNum);
		
		synchronized(Simulation.getLock(orderNum)) {
			//Wait until order progress is done
			while(Simulation.orderProgress(orderNum) == true) {
				try {
					Simulation.getLock(orderNum).wait();
				} catch (InterruptedException error) {
					error.printStackTrace();
				}
			}
		}
		
		//Once order progress is done, customer receives order
		Simulation.logEvent(SimulationEvent.customerReceivedOrder(this, order, orderNum));
		//Customer leaves ratsies
		Simulation.logEvent(SimulationEvent.customerLeavingRatsies(this));
		Simulation.leaveRatsies(this);
		
	}
}
