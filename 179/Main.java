

public class Main{

	final static int bound = 10*1000*1000;
	final static int[] arr = new int [bound+1];

	public static void main (String... args){

		for(int i=1; i <= bound; i++){
			arr[i] = 1;
		}

		for(int i=2; i <= bound; i++){
			for(int j=i; j <= bound; j += i){
				arr[j] ++;
			}
		}
		
		int result = 0;
		
		for(int i=2; i < bound; i++){
			if(arr[i] == arr[i+1])
				result ++;
		}
		
		System.out.println(bound);
		System.out.println(arr.length);		
		System.out.println(arr[bound]);
	/*	for(int i=0; i<=bound; i++){
			System.out.print(arr[i] + " ");
		}*/
		
		System.out.println(result);		
	}

	
}