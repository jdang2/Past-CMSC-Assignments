package cmsc433.p3;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveAction;

import cmsc433.p3.SkippingMazeSolver.SolutionFound;



/**
 * This file needs to hold your solver to be tested. 
 * You can alter the class to extend any class that extends MazeSolver.
 * It must have a constructor that takes in a Maze.
 * It must have a solve() method that returns the datatype List<Direction>
 *   which will either be a reference to a list of steps to take or will
 *   be null if the maze cannot be solved.
 */
public class StudentMTMazeSolver extends SkippingMazeSolver
{
	List<Direction> result = null;
	List<Direction> test;
	int numThreads;
	
    public StudentMTMazeSolver(Maze maze)
    {
        super(maze);
    }

    public List<Direction> solve()
    {
    	
    	RecursiveAction action;
    	numThreads = Runtime.getRuntime().availableProcessors();
    	ForkJoinPool pool = new ForkJoinPool(numThreads);
    	
    	try {
    		Choice start = firstChoice(maze.getStart());
    		int size = start.choices.size();
    		for(int i = 0; i < size; i++) {
    			Choice current = follow(start.at, start.choices.peek());
    			action = new solveAux(current, start.choices.pop());
    			pool.execute(action);
    			action.join();
    		}
    	}catch(SolutionFound sol) {
    		
    	}
    	
    	pool.shutdown();
    	return test;
    }
    
    @SuppressWarnings("serial")
	public class solveAux extends RecursiveAction{

    	Choice head;
    	Direction dir;
    	public solveAux(Choice head, Direction dir) {
    		this.head = head;
    		this.dir = dir;
    	}
    	
    	@Override
    	protected void compute() {
    		// TODO Auto-generated method stub
    		LinkedList<Choice> choiceStack = new LinkedList<Choice>();
            Choice ch;

            try
            {
                choiceStack.push(this.head);
                while (!choiceStack.isEmpty())
                {
                    ch = choiceStack.peek();
                    if (ch.isDeadend())
                    {
                        // backtrack.
                        choiceStack.pop();
                        if (!choiceStack.isEmpty()) choiceStack.peek().choices.pop();
                        continue;
                    }
                    choiceStack.push(follow(ch.at, ch.choices.peek()));
                }
                // No solution found.
                result = null;
            }
            catch (SolutionFound e)
            {
                Iterator<Choice> iter = choiceStack.iterator();
                LinkedList<Direction> solutionPath = new LinkedList<Direction>();
                while (iter.hasNext())
                {
                	ch = iter.next();
                    solutionPath.push(ch.choices.peek());
                }
                
                if (maze.display != null) {
                	maze.display.updateDisplay();
                }
                solutionPath.push(dir);
                result = pathToFullPath(solutionPath);
                test = result;
            }
    	}
    	
    }
}

 
