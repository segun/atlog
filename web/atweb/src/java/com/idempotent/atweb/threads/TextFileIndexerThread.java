/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.idempotent.atweb.threads;

import com.idempotent.atweb.utils.TextFileIndexer;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author aardvocate
 */
public class TextFileIndexerThread extends Thread {

    Path sourcePath;
    Properties p = new Properties();
    
    public TextFileIndexer indexer;
    
    static final Logger logger = Logger.getLogger(TextFileIndexerThread.class.getName());
    boolean isRunning;

    @Override
    public synchronized void start() {
        try {
            indexer = new TextFileIndexer();
            p.load(new FileReader(new File(System.getProperty("user.home"), "atweb.properties")));
            sourcePath = Paths.get(p.getProperty("source.location"));
            isRunning = true;
            super.start(); //To change body of generated methods, choose Tools | Templates.
        } catch (FileNotFoundException ex) {
            logger.log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            logger.log(Level.SEVERE, null, ex);
        }
    }

    @Override
    public void run() {
        while(isRunning) {
            try {
                logger.log(Level.INFO, "Start Indexing of Directory {0}", sourcePath.toString());               
                indexer.indexFileOrDirectory(sourcePath.toFile().getAbsolutePath());
                logger.log(Level.INFO, "Indexing of Directory {0} Completed Successfully", sourcePath.toString());
            } catch (Exception ex) {
                logger.log(Level.SEVERE, null, ex);                
            }
            
            try {
                Thread.sleep(Long.parseLong(p.getProperty("wait.time")));
            } catch (InterruptedException ex) {
                logger.log(Level.SEVERE, null, ex);
            }
        }
    }

    public synchronized void die() throws IOException {
        indexer.closeIndex();
        isRunning = false;        
    }

}
