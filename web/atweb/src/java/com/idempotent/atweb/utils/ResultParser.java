/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.idempotent.atweb.utils;

import com.idempotent.atweb.managedbeans.SearchBean;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author aardvocate
 */
public class ResultParser {

    enum FILETYPE {

        DIEBOLD, NCR, WINCOR, HYOSUNG
    }
    
    static final Logger logger = Logger.getLogger(ResultParser.class.getName());

    public String parse(File resultFile, String findByCardNumber) throws FileTypeNotFoundException, IOException {
        FILETYPE ft;

        String result = "";

        String fileName = resultFile.getName();

        if (fileName.endsWith(".txt") && !fileName.startsWith("ej_")) {
            ft = FILETYPE.DIEBOLD;
        } else if (fileName.startsWith("ej_")) {
            ft = FILETYPE.HYOSUNG;
        } else if (fileName.endsWith(".st1")) {
            ft = FILETYPE.NCR;
        } else if (fileName.endsWith("jrn")) {
            ft = FILETYPE.WINCOR;
        } else {
            throw new FileTypeNotFoundException("File type can not be determined from file name: " + fileName);
        }

        logger.log(Level.INFO, "Pasrsing {0} with Card Number {1}", new Object[] {ft, findByCardNumber});
        switch (ft) {
            case DIEBOLD:                
                result = parseDiebold(resultFile, "", findByCardNumber);
                break;
            case HYOSUNG:
                result = parseHyosung(resultFile, "", findByCardNumber);
                break;
            case NCR:
                result = parseNCR(resultFile, "", findByCardNumber);
                break;
            case WINCOR:
                result = parseWincor(resultFile, "", findByCardNumber);
                break;
        }

        return result;
    }

    private String parseWincor(File resultFile, String findByAccountNumber, String findByCardNumber) throws IOException {
        String transactionStart = "TRANSACTION START";
        String transactionEnd = "TRANSACTION END";
        String cardPrefix = " TRACK 2 DATA: ";
        boolean hasAccountNumber = false;
        boolean hasCardNumber = true;
        
        return parseFileType(resultFile, findByAccountNumber, findByCardNumber, transactionStart, transactionEnd, cardPrefix, hasAccountNumber, hasCardNumber);
    }

    private String parseHyosung(File resultFile, String findByAccountNumber, String findByCardNumber) throws FileNotFoundException, IOException {
        String transactionStart = "TRANSACTION START";
        String transactionEnd = "TRANSACTION END";
        String cardPrefix = "Card Number.*\\[";
        boolean hasAccountNumber = false;
        boolean hasCardNumber = true;
        
        return parseFileType(resultFile, findByAccountNumber, findByCardNumber, transactionStart, transactionEnd, cardPrefix, hasAccountNumber, hasCardNumber);
    }

    private String parseDiebold(File resultFile, String findByAccountNumber, String findByCardNumber) throws IOException {
        String transactionStart = "_new transaction";
        String transactionEnd = "_Card ejected";
        String cardPrefix = "_PIN ENTER Card ";
        boolean hasAccountNumber = false;
        boolean hasCardNumber = true;
        
        return parseFileType(resultFile, findByAccountNumber, findByCardNumber, transactionStart, transactionEnd, cardPrefix, hasAccountNumber, hasCardNumber);
    }

    private String parseNCR(File resultFile, String findByAccountNumber, String findByCardNumber) throws IOException {
        String transactionStart = "TRANSACTION STARTED";
        String transactionEnd = "TRANSACTION END";
        String cardPrefix = "CARD NUMBER............";
        boolean hasAccountNumber = false;
        boolean hasCardNumber = true;
        
        BufferedReader reader = new BufferedReader(new FileReader(resultFile));
        String line;
        List<StringBuilder> transactionBlocks = new ArrayList<StringBuilder>();
        StringBuilder transactionBlock = new StringBuilder();

        boolean append = false;
        boolean found = false;

        while ((line = reader.readLine()) != null) {
            if (append) {
                transactionBlock.append(line);
                transactionBlock.append("\n");
            }

            if (line.contains(transactionStart)) {
                transactionBlock = new StringBuilder();
                transactionBlock.append(resultFile.getAbsolutePath()).append("\n\n");
                transactionBlock.append(line);
                transactionBlock.append("\n");
                append = true;
            }

            if (!found && hasCardNumber) {
                String pattern = cardPrefix + "(\\d{4})";
                Pattern r = Pattern.compile(pattern);
                Matcher m = r.matcher(line);

                if (m.find()) {
                    if (m.group(1).equals(findByCardNumber)) {
                        found = true;
                    }
                }
            }
            
            if(!found && hasAccountNumber) {
                
            }

            if (line.contains(transactionEnd)) {
                append = false;
                transactionBlock.append(line);
                transactionBlock.append("\n");

                if (found) {
                    transactionBlocks.add(transactionBlock);
                    found = false;
                }
            }
        }

        String result = "";

        for (StringBuilder tb : transactionBlocks) {
            result += tb.toString();
            result += "\n-------------------------------------------\n";
        }

        return result;
    }

    
    private String parseFileType(File resultFile, String findByAccountNumber, String findByCardNumber, String transactionStart, String transactionEnd, String cardPrefix, boolean hasAccountNumber, boolean hasCardNumber) throws FileNotFoundException, IOException {
        BufferedReader reader = new BufferedReader(new FileReader(resultFile));
        String line;
        List<StringBuilder> transactionBlocks = new ArrayList<StringBuilder>();
        StringBuilder transactionBlock = new StringBuilder();

        boolean append = false;
        boolean found = false;

        while ((line = reader.readLine()) != null) {
            if (append) {
                transactionBlock.append(line);
                transactionBlock.append("\n");
            }

            if (line.contains(transactionStart)) {
                transactionBlock = new StringBuilder();
                transactionBlock.append(resultFile.getAbsolutePath()).append("\n\n");
                transactionBlock.append(line);
                transactionBlock.append("\n");
                append = true;
            }

            if (!found && hasCardNumber) {      
                String pattern = cardPrefix + "(\\d{4})(.*)(\\d{4})";
                Pattern r = Pattern.compile(pattern);
                Matcher m = r.matcher(line);

                if (m.find()) {
                    if (m.group(3).equals(findByCardNumber)) {
                        found = true;
                        logger.log(Level.INFO, "{0} : {1}", new Object[]{line, found});
                    }
                }
            }
            
            if(!found && hasAccountNumber) {
                
            }

            if (line.contains(transactionEnd)) {
                append = false;
                transactionBlock.append(line);
                transactionBlock.append("\n");

                if (found) {                    
                    transactionBlocks.add(transactionBlock);
                    found = false;
                }
            }
        }

        String result = "";

        int c = 1;
        int s = transactionBlocks.size();
        for (StringBuilder tb : transactionBlocks) {
            result += "******* Transaction " + c + " of " + s + " ******* \n\n" + tb.toString() ;
            result += "\n\n******* End Of Transaction " + c + " of " + s + " *******\n\n";
            c++;
        }

        return result;
    }    
}
