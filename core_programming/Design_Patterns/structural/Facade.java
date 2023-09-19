import java.math.BigDecimal;

// Facade class: wrapper class that encapsulates a subsystem
// in order to hide the subsystem's complexity

// Step 1: Design the interface

public interface IAccount {

  public void deposit(BigDecimal amount);

  public void withdraw(BigDecimal amount);

  public void transfer(BigDecimal amount);

  public int getAccountNumber(BigDecimal amount);

}

// Step 2: Implement the interface with one or more classes

public class Chequing implements IAccount {
  // ...
}

public class Saving implements IAccount {
  // ...
}

public class Investment implements IAccount {
  // ...
}

// Step 3: create the facade class and wrap the classes that implement the interface IAccount

public class BankService {
  private Hashtable<int, IAccount> bankAccounts;

  public BankService() {
    this.bankAccounts = new Hashtable<int, IAccount>;
  }

  public int createNewAccount(String type, BigDecimal initAmount) {

    IAccount newAccount = null;

    switch (type) {
      case "chequing":
        newAccount = new Chequing(initAmount);
        break;
      case "saving":
        newAccount = new Saving(initAmount);
        break;
      case "investment":
        newAccount = new Investment(initAmount);
        break;
      default:
        System.out.println("Invalid account type");
        break;
    }

    if (newAccount != null) {
      this.bankAccounts.put(newAccount.getAccountNumber(), newAccount);
      return newAccount.getAccountNumber();
    }

    return -1;
  }

  public void transferMoney(int to, int from, BigDecimal amount) {
    IAccount toAccount = this.bankAccounts.get(to);
    IAccount fromAccount = this.bankAccounts.get(from);
    fromAccount.transfer(toAccount, amount);
  }

}
