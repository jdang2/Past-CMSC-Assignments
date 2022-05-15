package cmsc433;

/**
 * Machines are used to make different kinds of Food. Each Machine type makes
 * just one kind of Food. Each machine type has a count: the set of machines of
 * that type can make that many food items in parallel. If the machines are
 * asked to produce a food item beyond its count, the requester blocks. Each
 * food item takes at least item.cookTime10S seconds to produce. In this
 * simulation, use Thread.sleep(item.cookTime10S) to simulate the actual cooking
 * time.
 */
public class Machines {

	public enum MachineType {
		sodaMachines, fryers, grillPresses, ovens
	};

	// Converts Machines instances into strings based on MachineType.
	public String toString() {
		switch (machineType) {
			case sodaMachines:
				return "Soda Machines";
			case fryers:
				return "Fryers";
			case grillPresses:
				return "Grill Presses";
			case ovens:
				return "Ovens";
			default:
				return "INVALID MACHINE TYPE";
		}
	}

	public final MachineType machineType;
	public final Food machineFoodType;
	public final int maxCapacity;
	//cookCapacity meant to represent how many cooks using a machine at a given time
	public int cookCapacity; 

	/**
	 * The constructor takes at least the name of the machines, the Food item they
	 * make, and their count. You may extend it with other arguments, if you wish.
	 * Notice that the constructor currently does nothing with the count; you must
	 * add code to make use of this field (and do whatever initialization etc. you
	 * need).
	 */
	public Machines(MachineType machineType, Food foodIn, int countIn) {
		this.machineType = machineType;
		this.machineFoodType = foodIn;
		this.maxCapacity = countIn;
		this.cookCapacity = 0;
	}

	/**
	 * This method is called by a Cook in order to make the Machines' food item. You
	 * can extend this method however you like, e.g., you can have it take extra
	 * parameters or return something other than Object. You will need to implement
	 * some means to notify the calling Cook when the food item is finished.
	 */
	public Thread makeFood(Food food){
		Thread t = new Thread(new CookAnItem(this, food));
		t.start();
		return t;
	}
	
	//Checks to see if max capacity of a machine is currently reached
	public boolean machineOccupied() {
		synchronized(this) {
			if(cookCapacity < maxCapacity) {
				return true;
			}else {
				return false;
			}
		}
	}
	
	//Returns the appropriate sleep time given a machine/food type
	public int getCookingTime(MachineType type) {
		if(type == MachineType.fryers) {
			return 30;
		}else if(type == MachineType.ovens) {
			return 90;
		}else if(type == MachineType.grillPresses) {
			return 50;
		}else if(type == MachineType.sodaMachines) {
			return 2;
		}else {
			throw new UnsupportedOperationException("Not a valid food type option");
		}
	}
	
	// THIS MIGHT BE A USEFUL METHOD TO HAVE AND USE BUT IS JUST ONE IDEA
	private class CookAnItem implements Runnable {
		Machines machine;
		Food food;
		
		public CookAnItem(Machines machine, Food food) {
			this.machine = machine;
			this.food = food;
		}
		
		public void run() {
			synchronized(machine) {
				//if machine is max capacity, wait until a cook stops using the machine
				while(!machineOccupied()) {
					try {
						machine.wait();
					}catch(InterruptedException error) {
						error.printStackTrace();
					}
				}
				//Machine is used to make food
				Simulation.logEvent(SimulationEvent.machinesCookingFood(machine, food));
				//Plus 1 for machine usage
				cookCapacity++;
			}
			
			try {
				//wait the given time
				Thread.sleep(getCookingTime(machineType));
			} catch(InterruptedException error) {
				error.printStackTrace();
			}
			
			synchronized(machine) {
				//Machine done cooking, reduce 1 for machine usage, notify Cook the food item is finished
				Simulation.logEvent(SimulationEvent.machinesDoneFood(machine, machineFoodType));
				cookCapacity--;
				machine.notifyAll();
			}
		}
	}
}
