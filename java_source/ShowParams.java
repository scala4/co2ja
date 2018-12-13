import com.heirloomcomputing.ecs.api.*;

//
// ShowParams
//
// Copyright (C) 2010-2017 Heirloom Computing Inc.  All Rights Reserved.
//
// This file and associated files are copyrighted information
// of Heirloom Computing.  Permission is granted for usage in 
// conjunction with the Elastic COBOL product.
//
// This Java program is useful for testing calls from Cobol
// to the Java environment.  It performs a visual dump of all
// call structure passed to sysout.  It can be used as the
// basis for any Java functionality desired to be accessed
// simply from Cobol.
// 
// See the definition of com.heirloomcomputing.ecs.api.IDatatype in the
// programmer's guide for more information.

public class ShowParams implements com.heirloomcomputing.ecs.api.ICallTarget
{
	/**
	 * call
	 *
	 * @param byRef each element is a parameter convention, 
	 *        true if by reference,
	 *        false if by content.
	 *        This may be null.
	 * @param param each element is a parameter value,
	 *        generally a com.heirloomcomputing.ecs.api.IDatatype implementor
	 *        unless the Cobol passes an object reference directly.
	 *        This may be null.
	 */
	public Object call(boolean byRef[],Object[] params)
	{
		// byRef may be null when across the network
		
		if(params!=null)
		{
			for(int i=0;i<params.length;i++)
			{
				System.out.print("Parameter "+i+": ");
				if(byRef!=null && byRef[i])
					System.out.println("(passed by reference)");
				else
					System.out.println("(passed by content)");
				show(i,params[i]);
				System.out.println("");
			}
		}
		else
			System.out.println("no params passed");
		return new Integer(0);
	}

	// Show with no indentation	
	private static void show(int num,Object param)
	{
		show(0,num,param);
	}
	
	// Space indentation of the given amount
	private static String indent(int num)
	{
		char[] result=new char[num];
		for(int i=0;i<num;i++) result[i]=' ';
		return new String(result);
	}
	
	// Show the parameter at the given level of indentation
	private static void show(int indent,int num,Object param)
	{
		try
		{
			IDatatype datatype=(IDatatype)param;
			int type=datatype.getType();
			Object elements[]=datatype.getElements();
			if(elements!=null)
			{
				if((type&com.heirloomcomputing.ecs.api.IDatatype.TYPE_CLASS_TABLE)!=0)
				{
					// table elements begin at 1, as in Cobol
					for(int i=1;i<elements.length;i++)
					{
						param=elements[i];
						
						try
						{
							datatype=(IDatatype)param;
							type=datatype.getType();
							System.out.println(indent(indent+2)+"Datatype: text='"+datatype.toText()+"', type='"+nameOfType(type)+"'");
						}
						catch(Throwable tableElementException)
						{
							System.out.println(indent(indent)+"'"+param+"'");
						}						
					}
				}
				else
				{
					// group elements begin at 0
					
					for(int i=0;i<elements.length;i++)
					{
						show(indent+2,num,elements[i]);
					}
				}
			}
			else
			{
				System.out.println(indent(indent)+"Datatype: text='"+datatype.toText()+"', type='"+nameOfType(type)+"'");
			}
		}
		catch(Throwable t)
		{
			System.out.println(indent(indent)+"'"+param+"'");
		}
	}
	
	private static String nameOfType(int type)
	{
		if(type<0) return "INVALID-DATATYPE";
		
		String result="";
		
		if((type&com.heirloomcomputing.ecs.api.IDatatype.TYPE_NUMERIC)==com.heirloomcomputing.ecs.api.IDatatype.TYPE_NUMERIC) 
		{
			switch(type&com.heirloomcomputing.ecs.api.IDatatype.TYPE_CLASS_MASK)
			{
				case com.heirloomcomputing.ecs.api.IDatatype.TYPE_CLASS_ZONED:	result+="ZONED "; break;
				case com.heirloomcomputing.ecs.api.IDatatype.TYPE_CLASS_BINARY:	result+="BINARY "; break;
				case com.heirloomcomputing.ecs.api.IDatatype.TYPE_CLASS_PACKED_DECIMAL:	result+="PACKED-DECIMAL "; break;
				case com.heirloomcomputing.ecs.api.IDatatype.TYPE_CLASS_FLOAT:	result+="FLOAT "; break;
			}
		}
		else 
		{
			switch(type&com.heirloomcomputing.ecs.api.IDatatype.TYPE_CLASS_MASK)
			{
				case com.heirloomcomputing.ecs.api.IDatatype.TYPE_CLASS_TEXT:	result+="TEXT "; break;
				case com.heirloomcomputing.ecs.api.IDatatype.TYPE_CLASS_GROUP:	result+="GROUP "; break;
				case com.heirloomcomputing.ecs.api.IDatatype.TYPE_CLASS_TABLE:	result+="TABLE "; break;
			}
		}
		
		switch(type&com.heirloomcomputing.ecs.api.IDatatype.TYPE_SIGN_MASK)
		{
			case com.heirloomcomputing.ecs.api.IDatatype.TYPE_SIGN_NONE:	result+="SIGN-NONE "; break;
			case com.heirloomcomputing.ecs.api.IDatatype.TYPE_SIGN_LEAD:	result+="SIGN-LEADING "; break;
			case com.heirloomcomputing.ecs.api.IDatatype.TYPE_SIGN_TRAIL:	result+="SIGN-TRAILING "; break;
			case com.heirloomcomputing.ecs.api.IDatatype.TYPE_SIGN_LEAD_SEP:	result+="SIGN-LEADING-SEPARATE "; break;
			case com.heirloomcomputing.ecs.api.IDatatype.TYPE_SIGN_TRAIL_SEP:	result+="SIGN-TRAILING-SEPARATE "; break;
		}

		if((type&com.heirloomcomputing.ecs.api.IDatatype.TYPE_JUST_RIGHT)==com.heirloomcomputing.ecs.api.IDatatype.TYPE_JUST_RIGHT) result+="JUSTIFIED ";
		
		int variant=type&com.heirloomcomputing.ecs.api.IDatatype.TYPE_VARIANT_7;
		if(variant>0)
		{
			variant>>=7;
			result+="VARIANT-"+variant;
		}
		
		return result.trim();
	}	
}

