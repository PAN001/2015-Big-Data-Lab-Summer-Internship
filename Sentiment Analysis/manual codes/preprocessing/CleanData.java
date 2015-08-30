import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.csvreader.CsvWriter;

/**
 * This is used to do data-cleaning work.
 * What's more, it provides a method capable of counting the frequency of each word.
 */
public class CleanData {
	
	private ArrayList<String[]> content;
	private Scanner input = new Scanner(System.in);
	private HashMap<String, Integer> wordCount = new HashMap<String,Integer>();
	private ArrayList<String> stopWords = readTxtFile("stopwords.txt");
	
	public String askForFileName() {
		System.out.println("Please enter the filename: ");
		return (input.nextLine());
	}
	
	public String askForColName() {
		System.out.println("Please enter the colName: ");
		return (input.nextLine());
	}
	
    /**
     * Read data from a txt file.
     * @param the path of the file
     * @return
     */
    public ArrayList<String> readTxtFile(String filePath){
    	ArrayList<String> content = new ArrayList<String>();
        try {
                String encoding="GBK";
                File file=new File(filePath);
                if(file.isFile() && file.exists()){ 
                    InputStreamReader read = new InputStreamReader(
                    new FileInputStream(file),encoding);
                    BufferedReader bufferedReader = new BufferedReader(read);
                    String lineTxt = null;
                    while((lineTxt = bufferedReader.readLine()) != null){
                        content.add(lineTxt);
                    }
                    read.close();
        }else{
            System.out.println("can not find the file");
            return null; 
        }
        } catch (Exception e) {
            System.out.println("there exists an error when reading the file");
            e.printStackTrace();
            return null;
        }
        return content;
    }
	
    /**
     * Read data from a csv file.
     * @param the path of the file
     * @return
     */
	public boolean readCsvFile() {
		String fileName = askForFileName();
		CsvUtil csv = null;
		try {
			csv = new CsvUtil(fileName);
		}
		catch(Exception FileNotFoundException) {
			System.out.println("Sorry, the file is not found");
			return false;
		}

		int rowNum = csv.getRowNum();
		content = new ArrayList<String[]>();
		content.add((String[]) csv.getHead(true));
		for(int i = 1;i < rowNum;i++) {
			String[] curRow = (String[]) csv.getRow(i, true);
			content.add(curRow);
		}
		return true;
	}
	
    /**
     * Remove the URL in the sentence.
     * @param the sentence to be processed
     * @return
     */
	public String removeURL(String s) {
		String stringPattern = "((http|ftp|https)://)(([a-zA-Z0-9\\._-]+\\.[a-zA-Z]{2,6})|([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))(:[0-9]{1,4})*(/[a-zA-Z0-9\\&%_\\./-~-]*)?";
		Pattern urlPattern = Pattern.compile(stringPattern);
		Matcher m = urlPattern.matcher(s);
		return m.replaceAll("");
	}
	
    /**
     * Remove punctutations in the sentence.
     * @param the sentence to be processed
     * @return
     */
	public String removePunctutations(String s) {
		String stringPattern = "\\p{Punct}";
		Pattern puncPattern = Pattern.compile(stringPattern);
		Matcher m = puncPattern.matcher(s);
		return m.replaceAll("");
	}
	
    /**
     * Remove user names and the htags in the sentence.
     * @param the sentence to be processed
     * @return
     */
	public String removeUnamesAndHtags(String s) {
		String stringPattern = "(\\@|\\#)(.+)\\:{1}";
		Pattern pattern = Pattern.compile(stringPattern);
		Matcher m = pattern.matcher(s);
		return m.replaceAll("");
	}
	
    /**
     * Remove stop words specified in the txt file
     * @param the sentence to be processed
     * @return
     */
	public String removeStopWords(String s) {
		Iterator<String> iterator = stopWords.iterator();
		while(iterator.hasNext()) {
			String stopWord = iterator.next();
			String stringPattern = " " + stopWord;
			Pattern swPattern = Pattern.compile(stringPattern);
			Matcher m = swPattern.matcher(s);
			s = m.replaceAll("");
		}
		return s;
	}
	
    /**
     * Remove words other than English and some rubbish.
     * @param the sentence to be processed
     * @return
     */
	public String removeRubbish(String s) {
		String stringPattern = "[^\\p{Alnum}, ]";
		Pattern pattern = Pattern.compile(stringPattern);
		Matcher m = pattern.matcher(s);
		return m.replaceAll("");
	}
	
    /**
     * Lower the words.
     * @param the sentence to be processed
     * @return
     */
	public String toLowerCase(String s) {
		return s.toLowerCase();
	}
	
    /**
     * Specify the column to be processed.
     * @param the name of the column
     * @return
     */
	public int specifyCollumn(String colName) {
		String[] header = content.get(0);
		int index = -1;
    	for(int i = 0;i < header.length;i++){
    		if(header[i].equals(colName)){
    			index = i;
   			}else{ // if it does not match the whole, but part of the column name
   				if(colName.startsWith(header[i])){
   					index = i;
   				}
   			}
   		}
    	return index;
	}
	
    /**
     * Write the cleaned data into a new csv file
     * @param the name of the new file
     * @return
     */
	public boolean writeIntoFile(String fileName) {
		boolean alreadyExists = new File(fileName).exists();
		try {
			// use FileWriter constructor that specifies open for appending
			CsvWriter csvOutput = new CsvWriter(new FileWriter(fileName, true), ',');
			
			// if the file didn't already exist then we need to write out the header line
			if (!alreadyExists)
			{
				Iterator<String[]> iterator = content.iterator();
				while(iterator.hasNext()) {
					for (String toWrite : iterator.next()) {
						csvOutput.write(toWrite);
					}
					csvOutput.endRecord();
				}
			}
			csvOutput.close();
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
    /**
     * Do word count job.
     * @param the sentence containing the words to be counted
     */
	public void countWord(String text) {
		String[] splitedText = text.split(" +");
		for(String curText:splitedText) {
			if(wordCount.containsKey(curText)) {
				int oldCount = wordCount.get(curText);
				wordCount.replace(curText, oldCount + 1);
			}
			else {
				wordCount.put(curText, 1);
			}
		}
	}
	
    /**
     * Print out the result of the word count in descending order.
     * @return
     */
	public void printWordCount() {
		List<Map.Entry<String, Integer>> list = new LinkedList<Map.Entry<String, Integer>>();
		list.addAll(wordCount.entrySet());
		Collections.sort(list, new Comparator<Map.Entry<String, Integer>>() {
		   public int compare(Map.Entry<String, Integer> obj1, Map.Entry<String, Integer> obj2) {//从高往低排序	     
		       if(obj1.getValue() < obj2.getValue())
		           return 1;
		       if(obj1.getValue() == obj2.getValue())
		           return 0;
		       else
		          return -1;
		   }
		});
		
		System.out.println("Word Count: ");
		int count = 1;
		for(Iterator<Map.Entry<String, Integer>> ite = list.iterator(); ite.hasNext();) {
		    Map.Entry<String, Integer> curPair = ite.next();
		    System.out.println(count + " " + curPair.getKey() + " : " + curPair.getValue());
		    count++;
		    if(count == 201) {
		    	break;
		    }
		}
	}
	
	public void printContent() {
		Iterator<String[]> iterator = content.iterator();
		while(iterator.hasNext()) {
			String[] next = iterator.next();
			for (int i = 0; i < next.length; i++) {
				System.out.print(next[i] + "!!!!!");
			}
			System.out.println("");
		}
	}
    /**
     * Start the cleaning job.
     */
	public boolean getCleaned() {
		if(readCsvFile()) {
			String colName = askForColName();
			int index = specifyCollumn(colName);
			if(index < 0) {
				return false;
			}
			System.out.print("Wheter to write into file(yes/no): ");
			String choice = input.nextLine();
			System.out.print("Only the specified column(yes/no): ");
			String choice2 = input.nextLine();
			String fileName = null;
			boolean alreadyExists = false;
			if(choice.equals("yes")) {
				System.out.print("Please enter the filename: ");
				fileName = input.nextLine();
				alreadyExists = new File(fileName).exists();
			}
			try {
				// use FileWriter constructor that specifies open for appending
				CsvWriter csvOutput = null;
				if(choice.equals("yes")) {
					csvOutput = new CsvWriter(new FileWriter(fileName, true), ',');
				}
//				printContent();
				// if the file didn't already exist then we need to write out the header line
				if (!alreadyExists)
				{
					Iterator<String[]> iterator = content.iterator();
					while(iterator.hasNext()) {
						String[] curText = iterator.next();
						if(curText.length < index) {
							continue;
						}
						String result;
						try {
							result = removeURL(curText[index]);
						}
						catch(IndexOutOfBoundsException e) {
							continue;
						}
						result = removeUnamesAndHtags(result);
						result = removePunctutations(result);
						result = removeStopWords(result);
						result = removeRubbish(result);
						result = toLowerCase(result);
						result = result.replaceAll("rt ", "");
						countWord(result);
						curText[index] = result;
						if(choice.equals("yes")) {
							if(!choice2.equals("yes")) {
								for (String toWrite : curText) {
									csvOutput.write(toWrite);
								}	
							}
							else {
								csvOutput.write(result);
							}
							csvOutput.endRecord();
						}
					}
					Runtime.getRuntime().exec("/Users/Erin/Desktop/Java/SummerIntern/stemwords -i " + "/Users/Erin/Desktop/Java/SummerIntern/" + fileName + " -o " + "/Users/Erin/Desktop/Java/SummerIntern/" + fileName + "_stemmed.csv");
					if(choice.equals("yes")) {
						csvOutput.close();
					}
				}
			} catch (IOException e) {
				e.printStackTrace();
				return false;
			}
			printWordCount();
			return true;
		}
		return false;
	}
}
