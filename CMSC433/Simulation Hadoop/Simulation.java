package cmsc433;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

/**
 * Simulation is the main class used to run the simulation. You may
 * add any fields (static or instance) or any methods you wish.
 */
public class Simulation {
	// List to track simulation events during simulation
	public static List<SimulationEvent> events;
	
	public static List<Customer> tableList; //List of customers currently sitting at a table
	public static HashMap<Food, Machines> machineList; //HashMap of machines and specified food
	public static HashMap<Integer, List<Food>> orderList; //HashMap of an order number + list of food (the order)
	public static HashMap<Integer, Object> locksPerOrder; //HashMap of an order number + its specific lock
	public static HashSet<Integer> newOrder; //HashSet to keep track of new orders being added by customers
	public static HashSet<Integer> orderInProgress; //HashSet to keep track of orders currently being cooked
	public static HashSet<Integer> finishedOrders; //HashSet to keep track of finished orders (orderNumbers)
	
	
	public static int customers; //Variable for number of customers
	public static int cooks; //Variable for number of cooks
	public static int tables; //Variable for max number of tables
	public static int machineCapacity; //Variable for machine capacity 
	public static boolean randomOrder;
	public static Object lock;
	
	
	/**
	 * Used by other classes in the simulation to log events
	 * 
	 * @param event
	 */
	public static void logEvent(SimulationEvent event) {
		events.add(event);
		System.out.println(event);
	}

	/**
	 * Function responsible for performing the simulation. Returns a List of
	 * SimulationEvent objects, constructed any way you see fit. This List will
	 * be validated by a call to Validate.validateSimulation. This method is
	 * called from Simulation.main(). We should be able to test your code by
	 * only calling runSimulation.
	 * 
	 * Parameters:
	 * 
	 * @param numCustomers the number of customers wanting to enter the restaurant
	 * @param numCooks the number of cooks in the simulation
	 * @param numTables the number of tables in the restaurant
	 * @param machineCount the count of all machine types in the restaurant
	 * @param randomOrders a flag say whether or not to give each customer a random
	 *        order
	 */
	public static List<SimulationEvent> runSimulation(
		int numCustomers, int numCooks,
		int numTables, int machineCount,
		boolean randomOrders) {
		// This method's signature MUST NOT CHANGE.
		
		customers = numCustomers;
		cooks = numCooks;
		tables = numTables;
		machineCapacity = machineCount;
		randomOrder = randomOrders;
		
		/*
		 * We are providing this events list object for you.
		 * It is the ONLY PLACE where a concurrent collection object is
		 * allowed to be used.
		 */
		events = Collections.synchronizedList(new ArrayList<SimulationEvent>());



		// Start the simulation
		logEvent(SimulationEvent.startSimulation(numCustomers,
			numCooks,
			numTables,
			machineCount));

		// Set things up you might need
		tableList = new ArrayList<Customer>(tables);
		machineList = new HashMap<Food, Machines>(machineCapacity);
		orderList = new HashMap<Integer, List<Food>>();
		locksPerOrder = new HashMap<Integer, Object>();
		newOrder = new HashSet<Integer>();
		orderInProgress = new HashSet<Integer>();
		finishedOrders = new HashSet<Integer>();
		
		// Start up machines
		machineList.put(FoodType.fries, new Machines(Machines.MachineType.fryers, FoodType.fries, machineCapacity));
		machineList.put(FoodType.pizza, new Machines(Machines.MachineType.ovens, FoodType.pizza, machineCapacity));
		machineList.put(FoodType.subs, new Machines(Machines.MachineType.grillPresses, FoodType.subs, machineCapacity));
		machineList.put(FoodType.soda, new Machines(Machines.MachineType.sodaMachines, FoodType.soda, machineCapacity));

		// Let cooks in
		Thread[] cookThreads = new Thread[numCooks];
		for(int i = 0; i < cookThreads.length; i++) {
			cookThreads[i] = new Thread(new Cook("Cook " + i, machineList));
		}


		// Build the customers.
		Thread[] customers = new Thread[numCustomers];
		LinkedList<Food> order;
		if (!randomOrders) {
			order = new LinkedList<Food>();
			order.add(FoodType.fries);
			order.add(FoodType.pizza);
			order.add(FoodType.subs);
			order.add(FoodType.soda);
			for (int i = 0; i < customers.length; i++) {
				customers[i] = new Thread(new Customer("Customer " + (i), order));
			}
		} else {
			for (int i = 0; i < customers.length; i++) {
				Random rnd = new Random();
				int friesCount = rnd.nextInt(4);
				int pizzaCount = rnd.nextInt(4);
				int subCount = rnd.nextInt(4);
				int sodaCount = rnd.nextInt(4);
				order = new LinkedList<Food>();
				for (int b = 0; b < friesCount; b++) {
					order.add(FoodType.fries);
				}
				for (int f = 0; f < pizzaCount; f++) {
					order.add(FoodType.pizza);
				}
				for (int f = 0; f < subCount; f++) {
					order.add(FoodType.subs);
				}
				for (int c = 0; c < sodaCount; c++) {
					order.add(FoodType.soda);
				}
				customers[i] = new Thread(
					new Customer("Customer " + (i), order));
			}
		}

		/*
		 * Now "let the customers know the shop is open" by starting them running in
		 * their own thread.
		 */
		for(int i = 0; i < cookThreads.length; i++) {
			cookThreads[i].start();
		}
		
		for (int i = 0; i < customers.length; i++) {
			customers[i].start();
			/*
			 * NOTE: Starting the customer does NOT mean they get to go right into the shop.
			 * There has to be a table for them. The Customer class' run method has many
			 * jobs to do - one of these is waiting for an available table...
			 */
		}

		try {
			/*
			 * Wait for customers to finish
			 * -- you need to add some code here...
			 */
			for(int i = 0; i < customers.length; i++) {
				customers[i].join();
			}
			
			//Wait until all orders are done before sending cooks home, so wait at this part of the code
			while(allOrdersDone() == false) {
				synchronized(lock) {
					try {
						lock.wait();
					} catch(InterruptedException error) {
						error.printStackTrace();
					}
				}
			}

			/*
			 * Then send cooks home...
			 * The easiest way to do this might be the following, where we interrupt their
			 * threads. There are other approaches though, so you can change this if you
			 * want to.
			 */
			for (int i = 0; i < cookThreads.length; i++)
				cookThreads[i].interrupt();
			for (int i = 0; i < cookThreads.length; i++)
				cookThreads[i].join();

		} catch (InterruptedException e) {
			System.out.println("Simulation thread interrupted.");
		}

		// Shut down machines
		Simulation.logEvent(SimulationEvent.machinesEnding(machineList.remove(FoodType.fries)));
		Simulation.logEvent(SimulationEvent.machinesEnding(machineList.remove(FoodType.pizza)));
		Simulation.logEvent(SimulationEvent.machinesEnding(machineList.remove(FoodType.subs)));
		Simulation.logEvent(SimulationEvent.machinesEnding(machineList.remove(FoodType.soda)));

		// Done with simulation
		logEvent(SimulationEvent.endSimulation());

		return events;
	}

	
	//Retrieve the specific lock to the given order number
	public static Object getLock(int orderNum) {
		synchronized(locksPerOrder){
			return locksPerOrder.get(orderNum);
		}
	}
	
	
	public static boolean orderOpen() {
		synchronized(newOrder) {
			if(newOrder.size() > 0){
				return true;
			}else {
				return false;
			}
		}
	}
	
	//Return the list of food of a given order number
	public static List<Food> getOrder(int orderNum){
		synchronized(orderList) {
			return orderList.get(orderNum);
		}
	}
	
	//Check if an order is completed 
	public static boolean orderProgress(int orderNum) {
		synchronized(getLock(orderNum)) {
			synchronized(finishedOrders) {
				return !finishedOrders.contains(orderNum);
			}
		}
	}
	
	//Check if all orders have been completed for the simulation
	public static boolean allOrdersDone() {
		if(finishedOrders.size() == customers) {
			return true;
		}else {
			return false;
		}
	}
	
	//Customer enters ratsies
	public static boolean enteredRatsies(Customer customer) {
		if(tableList == null || customer == null) {
			return false;
		}
		synchronized(tableList) {
			//No space for customer, thread must wait
			while(tableList.size() >= tables) {
				try {
					tableList.wait();
				}catch(InterruptedException error){
					error.printStackTrace();
				}
			}
			//Customer can now be added to table
			tableList.add(customer);
			return true;
		}
	}
	
	//Customer leaves ratsies
	public static boolean leaveRatsies(Customer customer) {
		if(tableList == null || !tableList.contains(customer) || customer == null) {
			return false;
		}
		synchronized(tableList) {
			//Remove customer from table, notify other threads that space has been made
			tableList.remove(customer);
			tableList.notify();
			tableList.notify();
		}
		return true;
	}
	
	//Submit an order with a specific lock
	public static boolean submitOrder(Customer customer, List<Food> order, int orderNum) {
		if(customer == null || order == null || orderList == null || newOrder == null) {
			return false;
		}
		
		synchronized(orderList) {
			orderList.put(orderNum, order);
		}
		
		synchronized(locksPerOrder) {
			locksPerOrder.put(orderNum, new Object());
		}
		
		synchronized(newOrder) {
			newOrder.add(orderNum);
			synchronized(Simulation.class) {
				Simulation.class.notify();
				Simulation.class.notify();
			}
			newOrder.notifyAll();
			return true;
		}
	}
	
	//Keeps track of order number
	public static Integer getNextOrderNum() {
		int orderNum = -1;
		
		synchronized(newOrder) {
			while(allOrdersDone() == false && newOrder.isEmpty()) {
				if(allOrdersDone() == true) {
					return null;
				}
				try {
					newOrder.wait();
				} catch (InterruptedException error) {
					error.printStackTrace();
				}
			}
			
			Iterator<Integer> iterator = newOrder.iterator();
			
			while(iterator.hasNext()) {
				orderNum = iterator.next();
				break;
			}
			newOrder.remove(orderNum);
			return orderNum;
		}
	}
	
	//Asks cook to start on an order 
	public static void startOrder(Cook cook, int orderNum) {
		synchronized(getLock(orderNum)) {
			synchronized(orderInProgress) {
				orderInProgress.add(orderNum);
				getLock(orderNum).notify();
				getLock(orderNum).notify();
			}
		}
	}
	
	//Cook has completed an order
	public static void completeOrder(Cook cook, int orderNum) {
		synchronized(Simulation.getLock(orderNum)) {
			synchronized(orderInProgress) {
				orderInProgress.remove(orderNum);
				
				synchronized(finishedOrders) {
					finishedOrders.add(orderNum);
				}
			}
			getLock(orderNum).notifyAll();
		}
	}
	/**
	 * Entry point for the simulation.
	 *
	 * @param args the command-line arguments for the simulation. There
	 *        should be exactly four arguments: the first is the number of
	 *        customers, the second is the number of cooks, the third is the number
	 *        of tables in the restaurant, and the fourth is the number of items
	 *        each set of machines can make at the same time.
	 */
	public static void main(String args[]) throws InterruptedException {
		// Command line argument parser

// @formatter:off
/*
 * 		if (args.length != 4) {
 * 			System.err.println("usage: java Simulation <#customers> <#cooks> <#tables> <count> <randomorders");
 * 			System.exit(1);
 * 		}
 * 		int numCustomers = new Integer(args[0]).intValue();
 * 		int numCooks = new Integer(args[1]).intValue();
 * 		int numTables = new Integer(args[2]).intValue();
 * 		int machineCount = new Integer(args[3]).intValue();
 * 		boolean randomOrders = new Boolean(args[4]);
 */
// @formatter: on
		
		// Parameters to the simulation
		int numCustomers = 10;
		int numCooks = 1;
		int numTables = 5;
		int machineCount = 4;
		boolean randomOrders = false;

		/* Run the simulation and then feed the result into the method to validate simulation. */
		System.out.println("Did it work? " +
			Validate.validateSimulation(
				runSimulation(
					numCustomers, numCooks,
					numTables, machineCount,
					randomOrders)));
	}

}
