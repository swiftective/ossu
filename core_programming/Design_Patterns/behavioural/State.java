// unintuitive way of doing State
public class VendingMachine {
  private State currentState;
  private int count;

  public VendingMachine(int count) {
    if (count > 0) {
      currentState = State.Idle;
      this.count = count;

    } else {
      currentState = State.OutOfStock;
      this.count = 0;

    }
  }

  // handle user events ...

  // handle insert dollar trigger
  public void insertDollar() {
    if (currentState == State.Idle) {
      currentState = State.HasOneDollar;

    } else if (currentState == State.HasOneDollar) {
      doReturnMoney();
      currentState = State.Idle;

    } else if (currentState == State.OutOfStock) {
      doReturnMoney();
    }
  }

  public void doReturnMoney() {
    // ...
  }

  public void ejectMoney() {
    // ...
  }

  public void dispense() {
    // ...
  }

  // ...
}

final class State { // singleton objects for states

  private State() {
    // ...
  }

  // all potential vending machine states as singleton
  public final static State Idle = new State();
  public final static State HasOneDollar = new State();
  public final static State OutOfStock = new State();

}

// State pattern
public interface IState {
  public void insertDollar(NewVendingMachine vendingMachine);

  public void ejectMoney(NewVendingMachine vendingMachine);

  public void dispense(NewVendingMachine vendingMachine);
}

public class NewVendingMachine {
  private IState idleState;
  private IState hasOneDollarState;
  private IState outOfStockState;

  private IState currentState;
  private int count;

  public NewVendingMachine(int count) {
    // make the needed states
    idleState = new IdleState();
    hasOneDollarState = new HasOneDollarState();
    outOfStockState = new OutOfStockState();

    if (count > 0) {
      currentState = idleState;
      this.count = count;

    } else {
      currentState = outOfStockState;
      this.count = 0;

    }
  }

  // handle user events ...

  // handle insert dollar trigger
  public void insertDollar() {
    currentState.insertDollar(this);
  }

  public void ejectMoney() {
    currentState.ejectMoney(this);
  }

  public void dispense() {
    currentState.dispense(this);
  }

  public void doReturnMoney() {
    // ...
  }

  public IState getHasOneDollarState() {
    // ...
  }

  public void setState(IState state) {
    // ...
  }

  public IState getIdleState() {
    // ...
  }

  public void doReleaseProduct() {
    // ...
  }

  public int getCount() {
    // ...
  }

  public IState getOutOfStockState() {
    // ...
  }

  // ...
}

public class IdleState implements IState {

  public void insertDollar(NewVendingMachine vendingMachine) {

    System.out.println("dollar inserted");
    vendingMachine.setState(vendingMachine.getHasOneDollarState());

  }

  public void ejectMoney(NewVendingMachine vendingMachine) {
    System.out.println("no money to return");
  }

  public void dispense(NewVendingMachine vendingMachine) {
    System.out.println("payment required");
  }

}

public class HasOneDollarState implements IState {

  public void insertDollar(NewVendingMachine vendingMachine) {
    System.out.println("already have one dollar");

    vendingMachine.doReturnMoney();
    vendingMachine.setState(vendingMachine.getIdleState());
  }

  public void ejectMoney(NewVendingMachine vendingMachine) {
    System.out.println("returning money");

    vendingMachine.doReturnMoney();
    vendingMachine.setState(vendingMachine.getIdleState());
  }

  public void dispense(NewVendingMachine vendingMachine) {
    System.out.println("releasing product");

    if (vendingMachine.getCount() > 1) {
      vendingMachine.doReleaseProduct();
      vendingMachine.setState(vendingMachine.getIdleState());

    } else {
      vendingMachine.doReleaseProduct();
      vendingMachine.setState(vendingMachine.getOutOfStockState());
    }
  }

}

public class OutOfStockState implements IState {

  public void insertDollar(NewVendingMachine vendingMachine) {
    // ...
  }

  public void ejectMoney(NewVendingMachine vendingMachine) {
    // ...
  }

  public void dispense(NewVendingMachine vendingMachine) {
    // ...
  }
}
