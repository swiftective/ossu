// Java program implementing Singleton class
// with using getInstance() method

// The primary purpose of a java Singleton class is to restrict the
// limit of the number of object creations to only one

public class Singleton {
  // Static variable reference of single_instance
  // of type Singleton
  private static Singleton single_instance = null;

  // Declaring a variable of type String
  public String s;

  // Constructor
  // Here we will be creating private constructor
  // restricted to this class itself
  private Singleton() {
    s = "Hello I am a string part of Singleton class";
  }

  // Static method
  // Static method to create instance of Singleton class
  public static synchronized Singleton getInstance() {
    if (single_instance == null) // lazy creation (the object is not created until necessary)
      single_instance = new Singleton();

    return single_instance;
  }
}
