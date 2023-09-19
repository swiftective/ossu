// the composite pattern

// Step 1: Design the interface that defines the overall type

import java.util.ArrayList;

public interface IStructure {

  public void enter();

  public void exit();

  public void location();

  public String getName();

}

// Step 2; Implement the composite class

public class Housing implements IStructure {

  private ArrayList<IStructure> structures;
  private String address;

  public Housing(String address) {
    this.structures = new ArrayList<IStructure>();
    this.address = address;
  }

  public String getName() {
    return this.address;
  }

  public int addStructure(IStructure component) {
    this.structures.add(component);
    return this.structures.size() - 1;
  }

  public void location() {
    System.out.println("You are currently in " + this.getName() + ". It has ");
    for (IStructure struct : this.structures)
      System.out.println(struct.getName());
  }

  public void enter() {
    // ...
  }

  public void exit() {
    // ...
  }

}

// Step 3: Implement the leaf class

public abstract class Room implements IStructure {

  public String name;

  public void enter() {
    System.out.println("You have entered the " + this.name);
  }

  public void exit() {
    System.out.println("You have left the " + this.name);
  }

  public void location() {
    System.out.println("You are currently in the " + this.name);
  }

  public String getName() {
    return this.name;
  }

}

// Main Program

public class Program {

  public static void main(String[] args) {

    Housing building = new Housing("123 Street");
    Housing floor1 = new Housing("123 Street - First Floor");
    int firstFloor = building.addStructure(floor1);

    // I know you cannot instantiate an abstract class but consider this
    // some sub-class of Room
    Room washroom1m = new Room("1F Men's Washroom");
    Room washroom1w = new Room("1F Women's Washroom");
    Room common1 = new Room("1F Common Area");

    int firstMens = floor1.addStructure(washroom1m);
    int firstWomans = floor1.addStructure(washroom1w);
    int firstCommon = floor1.addStructure(common1);

    building.enter(); // Enter the building
    Housing currentFloor = building.getStructure(firstFloor);
    currentFloor.enter(); // Walk into the first floor
    Room currentRoom = currentFloor.getStructure(firstMens); // Walk into the men's room
    currentRoom = currentFloor.getStructure(firstCommon);
    currentRoom.enter(); // Walk into the common area

  }

}
