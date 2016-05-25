import CLIPSJNI.*;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.Arrays;


public class IcterusDemo
{
  Environment clips = null;
  
  final static String EXIT_LOOP = "exit-loop";
  final static String TRUE = "TRUE";
  final static String FALSE = "FALSE";
  final static String[] YES_NO_ANSWERS = {"yes", "y", "no", "n"};
  
  Map<String, String> questions = null;
  
  
  IcterusDemo()
  {

    initQuestions();
    
    /* Creating the environment */
    clips = new Environment();

    /* Loading a .clp file */
    clips.load("icterus-simple.clp");

    /* Reset the environment */
    clips.reset();
    
  }

  void initQuestions()
  {
    questions = new HashMap<String, String>();
    
    questions.put("Has the patient got a fever?", "fever");
    questions.put("Has the patient yellowish eyes?", "yellowish-eyes");
    questions.put("Has the patient yellowish skin?", "yellowish-skin");
    questions.put("Is the patient stressed?", "stress");
    questions.put("Is the patient without food?", "without-food");
    questions.put("Is the patient young?", "young");
    questions.put("Is the patient tired?", "tired");
    questions.put("Has the patient been diagnosed dyspepsia?", "dyspepsia");
    questions.put("Has the patient's liver enlarged?", "enlarged-liver");
    questions.put("Has the patient recurrent pain?", "recurrent-pain");
    questions.put("Has the patient cholecyst pain?", "cholecyst-pain");
    questions.put("Does the patient use too much alcohol?", "alcohol-abuse");
    questions.put("Is the patient's spleen enlarged?", "enlarged-spleen");
  }

  String askQuestion(String question, List<String> possibleAnswers)
  {
    Scanner in = new Scanner(System.in);
     
    String response = "TRUE";
    String userAnswer = null;
    int answerIndex = -1;
    
    while (answerIndex < 0)
    {
      System.out.println(question);
      userAnswer = in.nextLine();
      answerIndex = possibleAnswers.indexOf(userAnswer);
    }
    
    if (answerIndex > 1)
    {
      response = "FALSE";
    }
    
    return response;
  }

  void assertSymptom(String symptomName, String observed)
  {
    String symptomFact = "(symptom (name " + symptomName + ") (observed " + observed + "))";

    clips.assertString(symptomFact);
  }

  /*
    We must throw an exception
   */
  String retrieveDiagnosis()
  {
    String diagnosis = null;
    try
    {
      PrimitiveValue fv = clips.eval("(get-all-facts-by-names diagnosis)").get(0);
      diagnosis = fv.getFactSlot("name").toString();
    }
    catch(Exception e)
    {
      /* do nothing */
    }
     
    return diagnosis;
  }

  String checkAgenda() throws Exception
  {
    return clips.eval("(agenda)").toString();
  }

  String listFacts() throws Exception
  {
    return clips.eval("(facts)").toString();
  }
  
  /**/
  void interact()
  {
    
    List<String> possibleAnswers = Arrays.asList(YES_NO_ANSWERS);

    /* For each question */
    for (Map.Entry<String, String> entry : questions.entrySet())
    {
      String response = askQuestion(entry.getKey(), possibleAnswers);

      /* asserting */
      assertSymptom(entry.getValue(), response);

      /** after each symptom **/
      /* inference */
      clips.run();

      /* getting the diagnosis */
      String diagnosis = retrieveDiagnosis();
      if (diagnosis != null)
      {
	System.out.println("\n\n--> The patient should be diagnosed: " + diagnosis);
	break;
      }    
    }
  }

  public static void main(String args[])
  {
    IcterusDemo classifierDemo = new IcterusDemo();
    classifierDemo.interact();
  }
}
