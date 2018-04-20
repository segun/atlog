/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.idempotent.atweb.managedbeans;

import com.idempotent.atweb.listeners.ContextListener;
import com.idempotent.atweb.models.SearchModel;
import com.idempotent.atweb.models.SearchResult;
import com.idempotent.atweb.utils.FileTypeNotFoundException;
import com.idempotent.atweb.utils.TextFileIndexer;
import static com.idempotent.atweb.utils.TextFileIndexer.FIELD_FILEPATH;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import lombok.Data;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.BooleanQuery.Builder;
import org.apache.lucene.search.NumericRangeQuery;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TermQuery;

/**
 *
 * @author aardvocate
 */
@ManagedBean
@SessionScoped
@Data
public class SearchBean implements Serializable {

    public SearchModel searchModel;
    TextFileIndexer indexer;

    SearchResult searchResult;
    String resultFile;

    static final Logger logger = Logger.getLogger(SearchBean.class.getName());

    /**
     * Creates a new instance of SearchBean
     */
    public SearchBean() {
        searchModel = new SearchModel();
        searchResult = new SearchResult();
        indexer = ContextListener.indexerThread.indexer;
    }

    public void poll() {
        logger.info("************************* POLLED *************************");
    }

    public void search() throws IOException, FileTypeNotFoundException, ParseException {
        searchResult = new SearchResult();
        List<String> result = new ArrayList<String>();
        int numResults = 0;
        String searchCardNumber = null;

        TermQuery cardNumberQuery = null;
        NumericRangeQuery dateRangeQuery = null;
        BooleanQuery andOrQuery = null;
        
        if(searchModel.getAndOr() == null) {
            searchModel.setAndOr("and");
        }

        if (searchModel.getCardNumber() != null && searchModel.getCardNumber().length() > 0) {
            searchCardNumber = searchModel.getCardNumber().replaceAll("XXXX XXXX XXXX ", "");
            cardNumberQuery = new TermQuery(new Term(TextFileIndexer.FIELD_CONTENTS, searchCardNumber));
        }

        if (searchModel.getStartDate() != null && searchModel.getStartDate().length() > 0) {
            if (searchModel.getEndDate() != null && searchModel.getEndDate().length() > 0) {
                long startDate = Long.parseLong(searchModel.getStartDate().replaceAll("/", ""));
                long endDate = Long.parseLong(searchModel.getEndDate().replaceAll("/", ""));
                dateRangeQuery = NumericRangeQuery.newLongRange(TextFileIndexer.FIELD_DATE, startDate, endDate, true, true);
            }
        }
        
        Builder builder = new Builder();
        if(cardNumberQuery != null) {
            if(searchModel.getAndOr().equals("and")) {
                builder.add(cardNumberQuery, BooleanClause.Occur.MUST);
            } else {
                builder.add(cardNumberQuery, BooleanClause.Occur.SHOULD);
            }
        }
        
        if(dateRangeQuery != null) {
            if(searchModel.getAndOr().equals("and")) {
                builder.add(dateRangeQuery, BooleanClause.Occur.MUST);
            } else {
                builder.add(dateRangeQuery, BooleanClause.Occur.SHOULD);
            }            
        }
        
        andOrQuery = builder.build();

        ScoreDoc[] hits = indexer.search(andOrQuery);

        for (ScoreDoc hit : hits) {
            int docId = hit.doc;
            Document d = indexer.indexSearcher.doc(docId);

            String filePath = d.get(FIELD_FILEPATH);
            String date = d.get(TextFileIndexer.FIELD_DATE);
            logger.log(Level.INFO, "File Path: {0}", filePath);
            logger.log(Level.INFO, "Date: {0}", date);
            String res = indexer.resultParser.parse(new File(filePath), searchCardNumber);
            if (res.length() > 0) {
                numResults++;                
                result.add(res);
                searchResult.setResults(result);
                searchResult.setNumResults(numResults);
            }
        }
    }
}
