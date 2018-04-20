/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.idempotent.atweb.utils;

/**
 *
 * @author aardvocate
 */
public class FileTypeNotFoundException extends Exception {
    String exception;

    public FileTypeNotFoundException(String exception) {
        super(exception);
    }        
}
