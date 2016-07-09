/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package lab.pkg2;

/**
 *
 * @author eeu213
 */

import java.util.*;

public class Lab2 {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        
        System.out.println("Decimal to Binary");
        System.out.println("***************** \n");
        
        Scanner in = new Scanner(System.in);
        
        System.out.println("Enter decimal number :> ");
        int decimal = in.nextInt();
        int[] answer = new int [32];
        while (decimal > 0)
        {
           int i = 0;
           answer[i] = decimal % 2; 
           decimal = decimal / 2;
           i = i + 1;
           System.out.println(decimal);
           System.out.println(Arrays.toString(answer))
        }
                
        System.out.println("The Binary representation of" + decimal + "is");
        ;
        
    }
}
