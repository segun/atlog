/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */


import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import java.io.IOException;

/**
 *
 * @author aardvocate
 */
public class ErlangParser {

    static String server = "atlog";
    OtpNode self = null;
    OtpMbox mbox = null;

    public ErlangParser() throws IOException {
        self = new OtpNode("mynode", "secret");
        mbox = self.createMbox("facserver");
    }

    public static void main(String[] args) throws IOException, OtpErlangExit, OtpErlangDecodeException {
        new ErlangParser().parse("/Users/aardvocates/Desktop/Journ/Diebold/1030003B_AAdemolaATM1__Diebold/20160519.txt");
    }
    
    public String parse(String fileName) throws OtpErlangExit, OtpErlangDecodeException {

        String type = "";

        if (fileName.toLowerCase().endsWith("st1") || fileName.toLowerCase().endsWith("dat")) {
            type = "hyosung";
        } else if (fileName.toLowerCase().endsWith("jrn")) {
            type = "wincor";
        } else if (fileName.toLowerCase().endsWith("txt")) {
            type = "diebold";
        }

        if (self.ping(server, 2000)) {
            OtpErlangObject[] msg = new OtpErlangObject[4];
            msg[0] = mbox.self();
            msg[1] = new OtpErlangAtom("parse");
            msg[2] = new OtpErlangAtom(type);
            msg[3] = new OtpErlangString(fileName);

            OtpErlangTuple tuple = new OtpErlangTuple(msg);
            mbox.send("atlog_fr", server, tuple);

            OtpErlangObject robj = mbox.receive();
            OtpErlangTuple rtuple = (OtpErlangTuple) robj;
            OtpErlangPid fromPid = (OtpErlangPid) (rtuple.elementAt(0));
            OtpErlangObject rmsg = rtuple.elementAt(1);

            OtpErlangBinary message = (OtpErlangBinary) rmsg;

            return new String(message.binaryValue());
        } else {
            System.err.println("Remote is not up");
            return "";
        }
    }
}
