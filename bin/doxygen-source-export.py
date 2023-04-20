#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   doxygen-source-export.py --- Doxygen Source file export
#
#   Copyright (C) 2023, Schspa Shi, all rights reserved.
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Imports
import argparse
import logging
import os
import re
import shutil
import subprocess
import sys
import shutil
from pprint import pprint
import xmltodict
import functools

base_dir = None

def BuildObjectFromKind(obj):

    pc = cparsers.get(obj['@kind'], None)
    if pc is None:
        print(f'Missing Parser for {obj["@kind"]}')

    return pc(obj)


def dict_to_latex_table(my_dict):
    table_str = "\\begin{tabular}{ |p{3cm}|p{8cm}| }\n"
    table_str += "\\hline\n"

    for key, value in my_dict.items():
        # Escape special characters in the value string
        value = value.replace("_", "\_").replace("&", "\&")
        if "\n" in value:
            num_rows = value.count("\n") + 1
            table_str += f"\\multirow{{{num_rows}}}{{*}}{{{key}}} & "
            table_str += value.replace("\n", f" \\\\ \\cline{{2-2}} & ")
        else:
            table_str += f"{key} & {value}"
        table_str += " \\\\ \n"

    table_str += "\\hline\n"
    table_str += "\\end{tabular}"
    return table_str

class DoxygenFunction():
    '''
      <memberdef kind="function" id="hb__bpu__scheduler_8h_1af0fe47211cfe042f1d885779259ae515" prot="public" static="no" const="no" explicit="no" inline="no" virt="non-virtual">
        <type>int32_t</type>
        <definition>int32_t hb_coor_scheduler_total_vms</definition>
        <argsstring>(void)</argsstring>
        <name>hb_coor_scheduler_total_vms</name>
        <param>
          <type>void</type>
        </param>
        <briefdescription>
        </briefdescription>
        <detaileddescription>
<para>hb_coor_scheduler_total_vms - Get total instance number</para>
<para>This function dispatches config info to Coordinator Host</para>
<para><simplesect kind="return"><para>0 on success, negative on failure. </para>
</simplesect>
</para>
        </detaileddescription>
        <inbodydescription>
        </inbodydescription>
        <location file="bpu_scheduler/hb_bpu_scheduler.h" line="12" column="9" declfile="bpu_scheduler/hb_bpu_scheduler.h" declline="12" declcolumn="9"/>
      </memberdef>
    '''

    def __init__(self, func):
        self.func = func
        self.params = {
        }
        self.item_parser = {
            'parameterlist' : self.get_params
        }
        recursionlimit = sys.getrecursionlimit()
        sys.setrecursionlimit(150000)
        self.parse_objects(self.func)
        sys.setrecursionlimit(recursionlimit)

    @property
    def name(self):
        return self.func['name']

    def parse_dict_or_list(self, objs, parser):
        res = []
        if isinstance(objs, list):
            for obj in objs:
                res.append(parser(obj))
        if isinstance(objs, dict):
            res.append(parser(objs))

        return res

    def parse_single_param(self, param):

        param_dir = 'in'
        param_detail = functools.reduce(lambda a, b: a+b, param['parameterdescription']['para'])
        param_name = param['parameternamelist']['parametername']

        if isinstance(param_name, dict):
            param_dir = param_name['@direction']
            param_name = param_name['#text']
        return {
            param_name : {
                'detail': param_detail,
                'dir': param_dir
            }
        }


    def parse_param(self, params):
        param_type = params['@kind']
        param_content = self.parse_dict_or_list(params['parameteritem'], self.parse_single_param)

        self.params[param_type] = param_content

    def get_params(self, parameterlist):
        #pprint(parameterlist)
        self.parse_dict_or_list(parameterlist, self.parse_param)

    def parse_objects(self, objs = None):
        if isinstance(objs, dict):
            for k, v in objs.items():
                parser = self.item_parser.get(k, None)
                if parser is None:
                    self.parse_objects(v)
                else:
                    parser(v)
        if isinstance(objs, list):
            for v in objs:
                self.parse_objects(v);

    def __str__(self):
        rep = f'Fcuntion: {self.name}\n\t' + str(self.params) + '\n'
        return rep

    def __expr__(self):
        return self.__str__()

    def html_tag_wrap(tn, tv):
        return f'<{tn}>{tv}</{tn}>'

    def to_html(self):

        finput = ''
        foutput = ''
        fcalls = "NA"
        fcalled = "NA"
        fother = "NA"
        fdesign = "NA"

        for param in self.params.get('param', []):

            for k, v in param.items():
                param_str = '{:s} :: {:s}'.format(k, v['detail'])
                if v['dir'] == 'in':
                    if finput != '':
                        finput += '<br>'
                    finput += param_str
                if v['dir'] == 'out':
                    if foutput != '':
                        foutput += '<br>'
                    foutput += param_str

        return '''
    <h6>{fname:s}<h6>
    <table>
      <tbody>
        <tr> <th colspan="2">{fname:s}</th></tr>
        <tr> <td>Description</td> <td>{fdesc:s}</td></tr>
        <tr> <td>ASIL</td> <td>B</td></tr>
        <tr> <td>INPUT</td> <td>{finput:s}</td></tr>
        <tr> <td>Output</td> <td>{foutput:s}</td></tr>
        <tr> <td>Calls</td> <td>{fcalls:s}</td></tr>
        <tr> <td>Called by</td> <td>{fcalled:s}</td></tr>
        <tr> <td>Others</td> <td>NA</td></tr>
        <tr> <td>Design</td> <td>{fdesign:s}</td></tr>
      </tbody>
    </table>
'''.format(fname = self.func['name'], fdesc = self.func['name'], finput = finput, foutput = foutput, fcalls = fcalls, fcalled = fcalled, fdesign = fdesign)
#         ts = '<h6>' + self.func['name'] + '</h6>\n'
#         ts += '''<table>\n'''
#         ts += '''\t<tr>\n\t\t<th>{:s}</th>\n\t\t<th>{:s}</th>\n\t</tr>\n'''.format('name', self.func['name'])
#         for k, params in self.params.items():
#             for param in params:
#                 for pk, v in param.items():
#                     ts += '''\t<tr>
#                     <td>{:s}</td>
#                     <td>{:s}</td>
#                     <td>{:s}</td>
# \t</tr>
#                     '''.format(k, pk, v)

#         ts += '''\n</table>'''
#         return ts

    def to_latex(self):
        finput = ''
        foutput = ''
        fcalls = "NA"
        fcalled = "NA"
        fother = "NA"
        fdesign = "NA"
        my_dict = {
            'Name' : self.func['name'],
            'Description' : '',
            'ASIL': 'B',
            'INPUT': '',
            'OUTPUT': '',
            'Calls' : 'NA',
            'Called': 'NA',
            'Others': 'NA',
            'Design': 'NA'
        }

        for param in self.params.get('param', []):

            for k, v in param.items():
                param_str = '{:s} :: {:s}'.format(k, v['detail'])
                key = 'INPUT'
                if v['dir'] == 'out':
                    key = 'OUTPUT'
                if my_dict[key] != '':
                    my_dict[key] += '\n'
                my_dict[key] += param_str


        ts = '\subsubsection{' + self.func['name'] + '}\n'
        return ts + dict_to_latex_table(my_dict)

class DoxygenSection():
    '''
<sectiondef kind="func">
    <memberdef kind="function" id="hb__bpu__scheduler_8h_1af0fe47211cfe042f1d885779259ae515" prot="public" static="no" const="no" explicit="no" inline="no" virt="non-virtual">
        <type>int32_t</type>
    </memberdef>
</sectiondef>
    '''

    def __init__(self, sec):
        # sec is a object
        self.data_dict = sec
        self.mems = []

        if isinstance(sec['memberdef'], dict):
            self.mems.append(BuildObjectFromKind(sec['memberdef'], ['function']))

        if isinstance(sec['memberdef'], list):
            for memb in self.data_dict['memberdef']:
                self.mems.append(BuildObjectFromKind(memb, ['function']))

    @property
    def kind(self):
        return self.data_dict['@kind']

    def __str__(self):
        rep = f'{self.kind}:'
        for mem in self.mems:
            rep = rep + str(mem)
        return rep

    def __expr__(self):
        return self.___str___()

    def to_latex(self):
        resp = '\subsection{' + self.kind + '}\n'
        for mem in self.mems:
            if mem is not None:
                resp += mem.to_latex()

        return resp

    def to_html(self):
        resp = '<h5>' + self.kind + '</h5>\n'
        for mem in self.mems:
            if mem is not None:
                resp += mem.to_html()
        return resp


class DoxygenFile():

    exclude_langs = [
        'Markdown',
        'Python'
    ]
    def __init__(self, f):
        xml_file = os.path.join(base_dir, f['@refid'] + '.xml')
        self.f = xml_file
        with open(xml_file) as f:
            self.data_dict = xmltodict.parse(f.read())

    def get_function(self):
        return None

    @property
    def language(self):
        return self.data_dict['doxygen']['compounddef']['@language']

    @property
    def sections(self):
        if self.language in self.exclude_langs:
            return []
        secs = []
        secdef = self.data_dict['doxygen']['compounddef']['sectiondef']
        if isinstance(secdef, list):
            for sec in secdef:
                secs.append(BuildObjectFromKind(sec, ['func']))

        if isinstance(secdef, dict):
            secs.append(BuildObjectFromKind(secdef, ['func']))
        return secs

    def to_latex(self):
        rep = ''
        for sec in self.sections:
            if sec is not None:
                rep += sec.to_latex()

        return rep

    def to_html(self):
        rep = ''
        for sec in self.sections:
            if sec is not None:
                rep += sec.to_html()

        return rep

    def __str__(self):
        rep = ''
        for sec in self.sections:
            if sec is not None:
                rep += sec.to_latex()

        return rep


def parse_file(f):
    df = DoxygenFile(f)
    print(df)

    return ''

cparsers = {
    'function' : DoxygenFunction,
    'file': DoxygenFile,
    'func': DoxygenSection,
}

def BuildObjectFromKind(obj, allowed_kind = None):

    kind = obj['@kind']
    if allowed_kind is not None:
        if kind not in allowed_kind:
            print(f'Parser for {kind} not allowed')
            return None

    pc = cparsers.get(kind, None)
    if pc is None:
        print(f'Missing Parser for {kind}')
        return None

    return pc(obj)

if __name__ == "__main__":

    import click
    import re

    def doxygen_xml_export(file_regrex, filename, export_type = 'html'):
        global base_dir
        base_dir = os.path.dirname(os.path.realpath(filename))
        with open(filename) as xml_file:
            data_dict = xmltodict.parse(xml_file.read())
            files = list(filter(lambda x: x['@kind'] == 'file', data_dict['doxygenindex']['compound']))
            for f in files:
                if re.match(file_regrex, f['name']):
                    fo = BuildObjectFromKind(f)
                    ef = getattr(fo, 'to_' + export_type, None)
                    if ef is not None:
                        print(ef())
                    else:
                        not_supported_preempt = {
                            'html': '<strong>export to html not supported!!</strong>',
                            'latex': '\textbf{export to latex not supported!!}'
                        }
                        print(fo.__class__.__name__ + not_supported_preempt[export_type])

    @click.command()
    @click.option('--file_regrex', default='^.*$', help='File filter.', type=str)
    @click.argument('filename', type=click.Path(exists=True))
    @click.option('--export_type',
                  type=click.Choice(['html', 'latex'], case_sensitive=False), default='html')
    def src_to_latex(file_regrex, filename, export_type):
        """Convert source code to latex."""
        logging.basicConfig(format='%(levelname)s[%(filename)s:%(funcName)s():%(lineno)s]: %(message)s', level=logging.DEBUG)


        if os.path.splitext(filename)[-1] != 'xml':
            import tempfile
            with tempfile.TemporaryDirectory() as temp_dir:
                with open(os.path.join(temp_dir, 'doxygen.cfg'), 'w') as f:
                    f.write('''
PROJECT_NAME     = "Doxygen Demo Project"
OUTPUT_DIRECTORY = {output_dir:s}
GENERATE_LATEX   = YES
GENERATE_MAN     = NO
GENERATE_RTF     = NO
CASE_SENSE_NAMES = NO
FILE_PATTERNS = *.c *.h
INPUT            = {file_path:s}
ENABLE_PREPROCESSING = YES
QUIET            = YES
JAVADOC_AUTOBRIEF = YES
JAVADOC_AUTOBRIEF = NO
GENERATE_HTML = NO
GENERATE_XML = YES
'''.format(output_dir = temp_dir, file_path = filename))
                retcode, output = subprocess.getstatusoutput('doxygen ' + os.path.join(temp_dir, 'doxygen.cfg'))
                if retcode != 0:
                    print("Doxygen process failed with status {:d} --> {:s}" % (retcode, output))
                    exit (-1)

                doxygen_xml_export(file_regrex, os.path.join(temp_dir, 'xml/index.xml'), export_type)


    src_to_latex()
