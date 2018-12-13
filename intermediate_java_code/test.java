/*
 * Elastic COBOL --> Java Source Code
 *
 * This is a cross-compiler generated Java source-code file whose original
 * source was a COBOL program of the same name.
 *
 * If you want to switch from COBOL maintenance to Java maintenance, you must:
 * 1. edit this file as you see fit,
 * 2. move the resultant file to the java_source folder, and
 * 3. delete the originally named COBOL source file.
 *
 * This program requires ecobol.jar and may also require etrans.jar to execute.
 * These are the Elastic COBOL runtime services (used under license).
 */

import com.heirloomcomputing.ecs.exec.*;

/*
 * PROGRAM-ID test
 * Compiled on 2018-12-13 at 10:17:37
 *
 * Generated by:
 * 
 * Elastic COBOL
 * V18.12.2 (Build Dec  2 2018 18:49:17)
 * Copyright (C) 2010-2018 Heirloom Computing
 */

@SuppressWarnings("all")
public class test extends CobolBase
implements ICobolProgram, com.heirloomcomputing.ecs.api.ICallTarget, com.heirloomcomputing.ecs.api.ICancelTarget 
{
    static public final String _programId="test";
    BreakClass breakPoint = new BreakClass();
    int _debugmarker = 0;
    
    public String toString()
    {
        return "PROGRAM TEST ("+super.toString()+"):"+String.valueOf(_context);
    }
    
    
    static // initializer
    {
        RuntimeEnvironment.setGlobalParameter("DT","0");
        Variable.completeInitialization();
    }
    
    public java.lang.Object call(Context _passContext)
    {
        setup(_passContext);
        return callGiving();
    }
    
    public java.lang.Object call(Context _passContext,parameterList _param)
    {
        setup(_passContext);
        return callGiving(_param);
    }
    
    public java.lang.Object callGiving()
    {
        call();
        return _context.getReturnCode();
    }
    
    public void call()
    {
        setup(_context);
        _context.setProgramCalled();
        run(-1,0);
    } // end call
    
    public java.lang.Object call(boolean[] _byRef,java.lang.Object[] _param)
    {
        return callGiving(new parameterList(_byRef,_param));
    }
    
    public java.lang.Object callGiving(parameterList _param)
    {
        call(_param);
        return _context.getReturnCode();
    }
    
    public void call(parameterList _param)
    {
        if(_context==null) setup(_context);
        _context.setProgramCalled();
        run(-1,0); // no parameters expected
    } // end call(parameterList)
    
    public int getElasticCobolMajorVersion() { return 18; }
    public int getElasticCobolMinorVersion() { return 12; }
    public int getElasticCobolSubminorVersion() { return 2; }
    public String getElasticCobolBuildTimestamp() { return "(Build Dec  2 2018 18:49:17)"; }
    
    
    public void run() // Runnable
    {
        try
        {
            run(-1,-1);
        }
        catch(UnwindException _unwindException)
        {
            return; // completely unwound
        }
        catch(Throwable _anyException)
        {
            CobolException.runtimeError(_context,_anyException);
        }
    }
    
    public void run(int _thread,int _entry) // ICobolInterface
    {
        if(_context==null)
        {
            if(_entry<0) _entry=0;
            setup(null);
        }
        else
        {
            if(_entry<0) _entry=_context.getEntryParagraph();
            _context.setEntryParagraph(0);
        }
        try
        {
            perform(_entry,-1);
            return;
        }
        catch(ExitProgramException _exitProgramException)
        {
            return; // goback
        }
    }
    
    public String redirectCall()
    {
        return null;
    }
    
    static public void main(String _args[]) // application entry
    {
        test _programInstance=null;
        try
        {
            _programInstance = new test();
            Context.initializeApplication(_programInstance,_args);
            _programInstance.run(-1,-1);
            _programInstance._context.exit();
        }
        catch(Throwable _throwableException)
        {
            if (_programInstance != null && _programInstance._context != null && _programInstance._context.getReturnInt() == 0) 
            {
                   _programInstance._context.setReturnCode(1);
                   Context.appReturnCode = _programInstance._context.getReturnInt();
            }
            CobolException.dumpError(_programInstance,_throwableException,false);
        }
        Context.applicationExit();
    }
    
    public transient Context _context;
    
    public Context setup(Context _passContext)
    {
        _context=_passContext;
        if(_context==null) // create fresh _context
        {
            _context=new Context(_passContext,_programId,this);
        }
        else
            _context.setupContext(_passContext,_programId,this);
        return _context;
    }
    
    public void cancel() // close open files, chance for custom action
    {
    } // end cancel
    
    
    public void close() // close any resources that might have been left open
    {
        if(_context.getExistingSqlContext() != null)
        {
            _context.getExistingSqlContext().getCursors().closeAll();
        }
    } // end close
    
    public void perform(int _procedure) // ICobolInterface
    {
        perform(_procedure,_procedure); // only procedure
    }
    
    public void perform(int _begin,int _end) // ICobolInterface
    {
        _debug_loadWatchpoints(_context.getProgramName());
        try
        {
            for(;;) // forever until stopped
            {
                switch(_begin)
                {
                    
                    case 0: // Initial Paragraph
                    _begin=-1; break; // starting paragraph
                    default: throw ExitProgramException.EXIT_DEFAULT;
                } // end switch
            } // end forever
        }
        catch(UnwindException _unwindException)
        {
            perform(_unwindException.unwind(_context,this),_end);
            return;
        }
        catch(ExitProgramException _exitProgramException)
        {
            throw _exitProgramException;
        }
        catch(Throwable _runtimeException)
        {
            int _resumeAt=_exceptionHandler(_runtimeException,false);
            if(_resumeAt>0) perform(_resumeAt,_resumeAt);
            else throw ExitProgramException.EXIT_RESUME;
        }
    } // end perform(int,int)
    
    public int _exceptionHandler(Throwable _exception,boolean _nextStatementAvailable)
    {
        $X$_exceptionInfo = String.valueOf(_debug_getExceptionLine(_exception));
        _exception.printStackTrace();
        // if debugging in Eclipse pause 
        CobolExceptionPause cbp = new CobolExceptionPause();
        $X$_exceptionInfo="";
        if(_exception instanceof RuntimeException) throw (RuntimeException)_exception;
        if(_exception instanceof Error) throw (Error)_exception;
        throw new com.heirloomcomputing.ecs.exception.EcObject(_exception);
    }
    
    static public final String _localNameConverter[][]=null;
    
} // end program test