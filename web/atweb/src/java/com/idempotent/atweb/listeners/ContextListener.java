/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.idempotent.atweb.listeners;

import com.idempotent.atweb.threads.TextFileIndexerThread;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

/**
 *
 * @author aardvocate
 */
public class ContextListener implements ServletContextListener {

    public static TextFileIndexerThread indexerThread;
    
    static final Logger logger = Logger.getLogger(ContextListener.class.getName());
    
    @Override
    public void contextInitialized(ServletContextEvent sce) {
        logger.info("Starting Indexer");
        indexerThread = new TextFileIndexerThread();
        indexerThread.start();
        logger.info("Indexer Started Successfully");
    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {
        try {
            logger.info("Killing Indexer");
            indexerThread.die();
            logger.info("Killed");
        } catch (IOException ex) {
            logger.log(Level.SEVERE, null, ex);
        }
    }
    
}
