import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * This is used to read data from a csv file.
 */
public class CsvUtil {
        private String fileName = null;
        private BufferedReader br = null;
        private List<String> list = new ArrayList<String>();
 
        /**
         * Constructor
         */
        public CsvUtil() {
 
        }
 
        /**
         * Constructor
         */
        public CsvUtil(String fileName) throws Exception {
                this.fileName = fileName;
                br = new BufferedReader(new FileReader(fileName));
                String stemp;
                while ((stemp = br.readLine()) != null) {
                        list.add(stemp);
                }
        }
 
        /**
         * Get the whole content
         * @return
         */
        public List<String> getList() {
                return list;
        }
        
        /**
         * Get the number of total rows
         * @return
         */
        public int getRowNum() {
                return list.size();
        }
        
        /**
         * Get the number of total columns
         * @return
         */
        public int getColNum() {
                if (!list.toString().equals("[]")) {
                        if (list.get(0).toString().contains(",")) {
                                return list.get(0).toString().split(",").length;
                        } else if (list.get(0).toString().trim().length() != 0) {
                                return 1;
                        } else {
                                return 0;
                        }
                } else {
                        return 0;
                }
        }
        
        /**
         * Get the first row, which is commonly the head
         * @param wetherSplit
         * @return
         */
        public Object getHead(boolean wetherSplit) {
        	if(!wetherSplit) {
                if (this.list.size() != 0) {
                        return (String) list.get(0);
                } else {
                        return null;
                }
        	}
        	else {
                if (this.list.size() != 0) {
                  return (String[]) list.get(0).split(",");
            } else {
                    return null;
            	}
        	}
    	}
        
        /**
         * Get the specific row,except for the first row
         * @param index
         * @param wetherSplit
         * @return
         */
        public Object getRow(int index, boolean wetherSplit) {
        	if(!wetherSplit) {
                if (this.list.size() != 0) {
                        return (String) list.get(index);
                } else {
                        return null;
                }
        	}
        	else {
                if (this.list.size() != 0) {
//                	return (String[]) list.get(index).split(",(?=((([^\"\"]*\"\"[^\"\"]*\"\")+[^\"]*)|([^\"]*\"{1}[^\"]*$)))");
                  return (String[]) list.get(index).split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)");
            } else {
                    return null;
            	}
        	}
        }
                
        /**
         * Get the specific column by index
         * @param index
         * @return
         */
        public String getCol(int index) {
                if (this.getColNum() == 0) {
                        return null;
                }
                StringBuffer sb = new StringBuffer();
                String tmp = null;
                int colnum = this.getColNum();
                if (colnum > 1) {
                        for (Iterator<String> it = list.iterator(); it.hasNext();) {
                                tmp = it.next().toString();
                                sb = sb.append(tmp.split(",")[index] + ",");
                        }
                } else {
                        for (Iterator<String> it = list.iterator(); it.hasNext();) {
                                tmp = it.next().toString();
                                sb = sb.append(tmp + ",");
                        }
                }
                String str = new String(sb.toString());
                str = str.substring(0, str.length() - 1);
                return str;
        }
        
        /**
         * Get the specific column by column name
         * @param colName
         * @return
         */
        public String getCol(String colName) {
        	String[] header = (String [])getRow(0, true);
        	int index = -1;
        	for(int i = 0;i < header.length;i++){
        		if(header[i].equals(colName)){
        			index = i;
       			}else{
       				if(colName.startsWith(header[i])){
       					index = i;
       				}
       			}
       		}
        	if(index < 0) {
        		return null;
        	}
        	else {
        		return getCol(index);
        	}
        }
                
        /**
         * Get the specific cell
         * @param row
         * @param col
         * @return
         */
        public String getString(int row, int col) {
                String temp = null;
                int colnum = this.getColNum();
                if (colnum > 1) {
                        temp = list.get(row).toString().split(",")[col];
                } else if(colnum == 1){
                        temp = list.get(row).toString();
                } else {
                        temp = null;
                }
                return temp;
        }
         
        public void CsvClose()throws Exception{
                this.br.close();
        }

}