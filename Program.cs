using System;
using System.Collections.Generic;

public class Program
{
    /* arr[] ---> Input Array 
      data[] ---> Temporary array to 
                  store current combination 
      start & end ---> Staring and Ending  
                       indexes in arr[] 
      index ---> Current index in data[] 
      r ---> Size of a combination 
              to be printed */
    static void comboNChooseKRecursive(int[] cList, int k, int[] buildList, int buildIndex, int start, int end, List<int[]> comboList)
    {
        // Current combination is  
        // ready to be printed,  
        // print it 
        if (buildIndex == k)
        {
            for (int j = 0; j < k; j++)
                Console.Write(buildList[j] + " ");
            var target = new int[k];
            buildList.CopyTo(target, 0);
            comboList.Add(target);
        }
        else
        {
            // replace index with all 
            // possible elements. The  
            // condition "end-i+1 >=  
            // r-index" makes sure that  
            // including one element 
            // at index will make a  
            // combination with remaining  
            // elements at remaining positions 
            //for (int i = start; i <= end &&
            //          end - i + 1 >= k - buildIndex; i++)
            for (int i = start; i <= end && buildIndex <= k; i++)
            {
                buildList[buildIndex] = cList[i];
                comboNChooseKRecursive(cList, k, buildList, buildIndex + 1, i + 1, end, comboList);
            }

        }
    }

    // The main function that prints 
    // all combinations of size r 
    // in arr[] of size n. This  
    // function mainly uses combinationUtil() 
    static void comboNChooseK(int[] cList, int n, int k)
    {
        // A temporary array to store  
        // all combination one by one 
        int[] buildList = new int[k];
        int buildIndex = 0;
        int start = 0;
        int end = n - 1;

        var comboList = new List<int[]>();

        // Print all combination  
        // using temprary array 'data[]' 
        comboNChooseKRecursive(cList, k, buildList, buildIndex, start, end, comboList);
    }

    public static void Main()
    {
        int[] cList = { 1, 2, 3, 4, 5 };
        int k = 3;
        int n = cList.Length;
        comboNChooseK(cList, n, k);
    }
}