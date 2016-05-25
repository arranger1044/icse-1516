import CLIPSJNI.*;

import java.util.Scanner;

public class ClipsREPL
{
  Environment clips = null;

  ClipsREPL()
  {
    /* Instantiating a new environment */
    clips = new Environment();

    /* clearing it */
    clips.clear();
  }

  void repl()
  {
    boolean endInteraction = false;

    Scanner in = new Scanner(System.in);
    
    while(!endInteraction)
    {
      System.out.print("CLIPS> ");
      /* Read */
      String userInput = in.nextLine();
      try
      {
	/* Eval */
	String response = clips.eval(userInput).toString();

	/* Print */
	System.out.println(response);

	/* Loop */
	if(response.equals("(exit)"))
	{
	  endInteraction = true;
	}
      }
      catch(Exception e)
      {
	e.printStackTrace();
      }
    }
  }

  public static void main(String args[])
  {
    ClipsREPL client = new ClipsREPL();
    client.repl();
  }
}
















