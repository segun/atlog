/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.idempotent.atweb.utils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Serializable;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.file.FileStore;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.UserDefinedFileAttributeView;
import java.util.ArrayList;
import java.util.Properties;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.LongField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

/**
 *
 * @author aardvocate
 */
public class TextFileIndexer implements Serializable {

    public ResultParser resultParser;

    public TextFileIndexer() throws IOException {
        resultParser = new ResultParser();
        createIndex();
    }

    private StandardAnalyzer analyzer;
    private IndexWriter writer;

    private final ArrayList<File> queue = new ArrayList<File>();

    String indexLocation;

    Properties p = new Properties();

    public QueryParser queryParser;
    public IndexSearcher indexSearcher;

    static final Logger logger = Logger.getLogger(TextFileIndexer.class.getName());

    public static final String FIELD_CONTENTS = "contents";
    public static final String FIELD_DATE = "date";
    public static final String FIELD_FILEPATH = "path";

    public static final int HITS_PER_PAGE = 1000;

    FSDirectory dir;

    private boolean createIndex() throws IOException {
        p.load(new FileReader(new File(System.getProperty("user.home"), "atweb.properties")));
        indexLocation = p.getProperty("index.location");
        File f = new File(indexLocation, "write.lock");
        if (f.exists()) {
            f.delete();
        }
        Path indexPath = Paths.get(indexLocation);
        Files.createDirectories(indexPath);
        dir = FSDirectory.open(indexPath);
        return true;
    }

    private void addAttribute(Path file) throws IOException {
        FileStore store = Files.getFileStore(file);
        if (!store.supportsFileAttributeView(UserDefinedFileAttributeView.class)) {
            //logger.log(Level.SEVERE, "UserDefinedFileAttributeView not supported on {0}", store);
        } else {
            UserDefinedFileAttributeView view = Files.getFileAttributeView(file, UserDefinedFileAttributeView.class);
            view.write("user.indexed", Charset.defaultCharset().encode("1"));
        }
    }

    private boolean isIndexed(Path file) throws IOException {
        FileStore store = Files.getFileStore(file);
        if (!store.supportsFileAttributeView(UserDefinedFileAttributeView.class)) {
            logger.log(Level.SEVERE, "UserDefinedFileAttributeView not supported on {0}", store);
            //TODO: Delete this line.
            //return true;

            return false;
        } else {
            UserDefinedFileAttributeView view = Files.getFileAttributeView(file, UserDefinedFileAttributeView.class);

            String name = "user.indexed";
            try {
                int size = view.size(name);
                ByteBuffer buf = ByteBuffer.allocateDirect(size);
                view.read(name, buf);
                buf.flip();
                String indexed = Charset.defaultCharset().decode(buf).toString();
                return indexed.equals("1");
            } catch (Exception e) {
                logger.severe(e.getMessage());
                return false;
            }
        }
    }

    public boolean indexFileOrDirectory(String filePath) throws FileNotFoundException, IOException, FileTypeNotFoundException {
        addFilesToQueue(new File(filePath));

        if (writer == null || !writer.isOpen()) {
            analyzer = new StandardAnalyzer();
            IndexWriterConfig config = new IndexWriterConfig(analyzer);
            writer = new IndexWriter(dir, config);
        }

        if (queryParser == null) {
            createReader();
        }

        if (writer == null || !writer.isOpen()) {
            analyzer = new StandardAnalyzer();
            IndexWriterConfig config = new IndexWriterConfig(analyzer);
            writer = new IndexWriter(dir, config);
        }

        int originalNumDocs = writer.numDocs();

        logger.log(Level.INFO, "{0} documents existing.", originalNumDocs);

        for (File f : queue) {

            long date = -1;
            String fileName = f.getName();
            if (fileName.endsWith(".txt") && !fileName.startsWith("ej_")) {
                //It's Diebold, Date is in filename
                date = Long.parseLong(fileName.replaceAll(".txt", ""));
            } else if (fileName.startsWith("ej_") && fileName.endsWith(".txt")) {
                //It's Hyosung Type 2, Date is in file 
                //[2014/11/16 18:59:35]TRANSACTION START
                String regex = "\\[(\\d{4}/\\d{2}/\\d{2}).*\\]TRANSACTION START";
                date = getDate(f, regex, true);
            } else if (fileName.startsWith("ej_") && fileName.endsWith(".dat")) {
                //It's Hyosung Type 1, Date is in file 
                //14/06/2013 12:32:07 TRANSACTION DATA                
                String regex = "(\\d{2}/\\d{2}/\\d{4}).*TRANSACTION DATA.*";
                date = getDate(f, regex, false);
            } else if (fileName.endsWith(".st1")) {
                //It's NCR, Date is in file
                //    31\07\15     06:26     10844172
                String regex = "(\\d{2}/\\d{2}/\\d{4}).*";
                date = getDate(f, regex, false);
            } else if (fileName.endsWith("jrn")) {
                //It's Wincor, Date is in filename
                date = Long.parseLong(fileName.replaceAll(".jrn", ""));
            } else {
                //throw new FileTypeNotFoundException("File type can not be determined from file name: " + fileName);
            }

            if (!isIndexed(Paths.get(f.getAbsolutePath()))) {
                FileReader fr = new FileReader(f);
                Document doc = new Document();

                doc.add(new TextField(FIELD_CONTENTS, fr));
                doc.add(new StringField(FIELD_FILEPATH, f.getPath(), Field.Store.YES));
                doc.add(new LongField(FIELD_DATE, date, Field.Store.YES));

                writer.addDocument(doc);

                addAttribute(Paths.get(f.getAbsolutePath()));
            }
        }

        int newNumDocs = writer.numDocs();

        logger.log(Level.INFO, "{0} documents added.", (newNumDocs - originalNumDocs));

        queue.clear();
        writer.flush();
        writer.commit();
        writer.close();

        return true;
    }

    public void addFilesToQueue(File file) {
        if (file.isDirectory()) {
            for (File f : file.listFiles()) {
                addFilesToQueue(f);
            }
        } else {
            queue.add(file);
        }
    }

    public boolean closeIndex() throws IOException {
        if (writer != null && writer.isOpen()) {
            writer.flush();
            writer.commit();
            writer.close();
        }
        return true;
    }

    public ScoreDoc[] search(BooleanQuery query) throws IOException, ParseException {
        logger.info("Started Search");
        TopDocs docs = indexSearcher.search(query, HITS_PER_PAGE);
        ScoreDoc hits[] = docs.scoreDocs;

        logger.log(Level.INFO, "HITS: {0}", hits.length);

        return hits;
    }

    private void createReader() throws IOException {
        if (writer != null && writer.isOpen()) {
            writer.flush();
            writer.commit();
            writer.close();
        }

        Directory index = FSDirectory.open(Paths.get(indexLocation));
        IndexReader indexReader = DirectoryReader.open(index);
        indexSearcher = new IndexSearcher(indexReader);

        queryParser = new QueryParser(FIELD_CONTENTS, analyzer);
    }

    private long getDate(File f, String regex, boolean yearFirst) throws FileNotFoundException {
        Pattern r = Pattern.compile(regex);
        Scanner scanner = new Scanner(f);
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (f.getName().contains(".st1")) {
                line = line.replace("\\", "/");
            }
            //Matcher m = r.matcher("[2014/12/03 07:50:13]TRANSACTION START");
            Matcher m = r.matcher(line);
            if (m.find()) {
                if (yearFirst) {
                    return Long.parseLong(m.group(1).replaceAll("/", ""));
                } else if (!yearFirst) {
                    String[] splitted = m.group(1).split("/");
                    if (f.getName().endsWith(".st1")) {
                        //NCR MM/DD/YY
                        return Long.parseLong(splitted[2] + splitted[0] + splitted[1]);
                    } else {
                        //Hyosung DD/MM/YY                    
                        return Long.parseLong(splitted[2] + splitted[1] + splitted[0]);
                    }
                }
            }
        }

        return -1;
    }
}
