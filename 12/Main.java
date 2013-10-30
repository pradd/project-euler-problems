
/**
The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

 1: 1
 3: 1,3
 6: 1,2,3,6
10: 1,2,5,10
15: 1,3,5,15
21: 1,3,7,21
28: 1,2,4,7,14,28
We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred divisors?
*/
public class Main{

	final static int MIN_DIVISORS = 500+1;
	final static int SIEVE_BOUND = 100*1000*1000;
	final static int[] sieve = new int [SIEVE_BOUND + 1];

	public static void main (String... args){

		// prepare sieve
		for(int i=1; i <= SIEVE_BOUND; i++){
			sieve[i] = 1;
		}
		for(int i=2; i <= SIEVE_BOUND; i++){
			for(int j=i; j <= SIEVE_BOUND; j += i){
				sieve[j] ++;
			}
		}
		
		
		
		int n = 1;
		int triangleNumber = (1+n)*n/2;
		
		while(triangleNumber < SIEVE_BOUND){
			if(sieve[triangleNumber] >= MIN_DIVISORS){
				System.out.println(triangleNumber);
				break;
			}
			n++;
			triangleNumber = (1+n)*n/2;
		}
		
		System.out.println("Finished");
	}

	
}