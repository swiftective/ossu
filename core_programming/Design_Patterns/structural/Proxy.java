// virtual proxy: light-weight class
// remote proxy: represents a remote subject
// protection proxy: control access to a sensitive real subject

// Step 1: Design the subject interface

import java.util.Hashtable;
import java.util.List;

public interface IOrder {
  public void fulfillOrder(Order);
}

// Step 2: Implement the real subject class

public class Warehouse implements IOrder {

  private Hashtable<String, Integer> stock;
  private String address;

  // Constructors and other attributes would go here
  // ...

  public void fulfillOrder(Order order) {

    for (Item item : order.itemList) {
      this.stock.replace(item.sku, stock.get(item) - 1);
    }

    // Process the order for shipment and delivery
    // ...

  }

  public int currentInventory(Item item) {

    if (stock.containsKey(item.sku)) {
      return stock.get(item.sku).intValue();
    }

    return 0;

  }

}

public class OrderFulfillment implements IOrder {

  private List<Warehouse> warehouses;

  // Constructors and other attributes world go here

  public void fulfillOrder(Order order) {
    // For each item in a customer order, check each warehouse
    // to see if it is in stock

    // If it is then create a new Order for that warehouse. Else
    // check the next warehouse.

    // Send all the Orders to the warehouse(s) after you finish iterating
    // over all the items in the original Order.

    for (Item item : order.itemList) {

      for (Warehouse warehouse : warehouses) {
        // ...
      }

    }

    return;

  }

}
