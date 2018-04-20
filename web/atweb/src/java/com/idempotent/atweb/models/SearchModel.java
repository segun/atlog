/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.idempotent.atweb.models;

import lombok.Data;

/**
 *
 * @author aardvocate
 */
@Data
public class SearchModel {
    private String cardNumber;
    private String startDate;
    private String endDate;
    private String andOr;
}
