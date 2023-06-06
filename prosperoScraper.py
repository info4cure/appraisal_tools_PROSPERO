#!/usr/bin/env python3

# read metadata from prospero repository
# currently only 'clinical' records are accepted
# 'pre-clinical' and 'cochrane' are not supported
# (as we don't know what data to extract from them)


import functools
import json
import urllib.request
import time
import sys

class Record(object):
    def __init__(self, record):        
        assert record['recordtype'].strip().lower() == "clinical"
        self.__record = record

    def __get_field(self, qidx, idx=0):
        p = self.__record['fields'][qidx-1]['content']
        return p if idx is None else (p[idx]['text'] if idx < len(p) else None)

    def __get_field_all(self, qidx):
        return functools.reduce(
            lambda x, y: '{}{}'.format(x, y),
            map(lambda x: '<p>{}</p>'.format(x['text']), self.__record['fields'][qidx-1]['content']))

    @property
    def id(self):
        return self.__record['recordid']

    @property
    def authors(self):
        return functools.reduce(
            lambda x, y: '{}, {}'.format(x, y),
            map(lambda x: '{} {}'.format(x['firstname'].strip(), x['lastname'].strip()),
                self.__get_field(11)))

    @property
    def title(self):
        return self.__get_field(1)

    @property
    def accession_number(self):
        return self.__record['crdaccessionnumber']

    @property
    def database_name(self):
        return self.__record['databasename']

    @property
    def citation(self):
        link = 'http://www.crd.york.ac.uk/PROSPERO/display_record.php?ID={}'.format(self.accession_number)
        return '{}{}{} {} {} Available from: <a href="{}">{}</a>'.format(
            (self.authors + '. ') if self.authors else '',
            (self.title + '. ') if self.title else '',
            self.database_name,
            self.accession_number[4:8],
            self.accession_number,
            link,
            link)

    @property
    def review_question(self):
        return self.__get_field(15)

    @property
    def condition_or_domain_being_studied(self):
        return self.__get_field(18)

    @property
    def intervention_exposure(self):
        return self.__get_field(20)

    @property
    def data_extraction(self):
        return self.__get_field(26)

    @property
    def quality_assessment(self):
        return self.__get_field(27)

    @property
    def start_date(self):
        return self.__get_field(3)

    @property
    def completion_date(self):
        return self.__get_field(4)

    @property
    def funding_sources(self):
        return self.__get_field(12)

    @property
    def conflict(self):
        return self.__get_field(13, 1) or self.__get_field(13, 0)

    @property
    def language(self):
        return self.__get_field(31)

    @property
    def country(self):
        return self.__get_field(32)

    @property
    def date_of_registration(self):
        return self.__record['datefirstpublished']

    @property
    def date_of_publication(self):
        return self.__record['datepublished']

    # TODO: fix this
    @property
    def stage_of_review(self):
        other = self.__get_field(38, 1)        
        if other and other.endswith('<br />'):
            other = other[:-6]
        if other and other.endswith('<br/>'):
            other = other[:-5]
        return '{}{}'.format(
            self.__get_field(38),
            ' ({})'.format(other) if other else '')

    @property
    def stage_of_review_table(self):
        xs = self.__get_field(5, None)

        output = ''

        if Record.__getQ5Status('Review_not_Started', xs) == "Yes":
            output += "<p>The review has not started</p>"

        output += '<table class="stageofreviewtable">'
        output += '<thead>'
        output += '<tr>'
        output += '<th class="stageofreviewtable_stage col1">Stage</th>'
        output += '<th class="stageofreviewtable_started col2">Started</th>'
        output += '<th class="stageofreviewtable_completed col3">Completed</th>'
        output += '</tr>'
        output += '</thead>'
        output += '<tbody>'
        output += '<tr>'
        output += '<td class="col1">Preliminary searches</td>'
        output += '<td class="col2">' + Record.__getQ5Status('Preliminary_Started', xs) + '</td>'
        output += '<td class="col3">' + Record.__getQ5Status('Preliminary_Completed', xs) + '</td>'
        output += '</tr>'

        output += '<tr>'
        output += '<td class="col1">Piloting of the study selection process</td>'
        output += '<td class="col2">' + Record.__getQ5Status('Study_Started', xs) + '</td>'
        output += '<td class="col3">' + Record.__getQ5Status('Study_Completed', xs) + '</td>'
        output += '</tr>'

        output += '<tr>'
        output += '<td class="col1">Formal screening of search results against eligibility criteria</td>'
        output += '<td class="col2">' + Record.__getQ5Status('Formal_Screening_Started', xs) + '</td>'
        output += '<td class="col3">' + Record.__getQ5Status('Formal_Screening_Completed', xs) + '</td>'
        output += '</tr>'

        output += '<tr>'
        output += '<td class="col1">Data extraction</td>'
        output += '<td class="col2">' + Record.__getQ5Status('Data_Extraction_Started', xs) + '</td>'
        output += '<td class="col3">' + Record.__getQ5Status('Data_Extraction_Completed', xs) + '</td>'
        output += '</tr>'

        output += '<tr>'
        output += '<td class="col1">Risk of bias (quality) assessment</td>'
        output += '<td class="col2">' + Record.__getQ5Status('Quality_Assessment_Started', xs) + '</td>'
        output += '<td class="col3">' + Record.__getQ5Status('Quality_Assessment_Completed', xs) + '</td>'
        output += '</tr>'

        output += '<tr>'
        output += '<td class="col1">Data analysis</td>'
        output += '<td class="col2">' + Record.__getQ5Status('Analysis_Started', xs) + '</td>'
        output += '<td class="col3">' + Record.__getQ5Status('Analysis_Completed', xs) + '</td>'
        output += '</tr>'

        output += '</tbody>'
        output += '</table>'

        return output

    @staticmethod
    def __getQ5Status(field, xs):
        for x in xs:
            if x['sub'] == field:
                return 'Yes' if x['text'] and x['text'] != 'No' else 'No'
        return ''

    @staticmethod
    def __convert_to_row(xs):
        return functools.reduce(
            lambda x, y: '{},{}'.format(x, y),
            map(lambda x: '"{}"'.format(x.replace('"', '\\"') if x else ''), xs)
        )

    @staticmethod
    def title_row():
        return Record.__convert_to_row([
            'Id',
            'Citation',
            'Review question',
            'Condition or domain being studied',
            'Intervention(s), exposure(s)',
            'Data extraction (selection and coding)',
            'Risk of bias (quality) assessment',
            'Anticipated or actual start date',
            'Anticipated completion date',
            'Funding sources/sponsors',
            'Conflicts of interest',
            'Language',
            'Country',
            'Stage of review',
            'Date of registration in PROSPERO',
            'Date of publication of this version',
            'Stage of review at time of this submission'
        ])

    @property
    def as_row(self):
        return Record.__convert_to_row([
            self.id,
            self.citation,
            self.review_question ,
            self.condition_or_domain_being_studied ,
            self.intervention_exposure ,
            self.data_extraction ,
            self.quality_assessment ,
            self.start_date ,
            self.completion_date ,
            self.funding_sources ,
            self.conflict ,
            self.language ,
            self.country ,
            self.stage_of_review ,
            self.date_of_registration ,
            self.date_of_publication ,
            self.stage_of_review_table
        ])

def read_prospero_record(id):    
    url = 'https://www.crd.york.ac.uk/prospero/ajax.php?url=/record/getrecord/{}/0/View'.format(id)
    response = urllib.request.urlopen(url)
    data = response.read()
    text = data.decode('utf-8')
    records = json.loads(text)
    assert 1 == len(records)
    return records[0]

def print_usage():
    print("Formato de uso: prosperoScraper id1 id2 id3 id4-id5 id6, con idn entero.")

def error_on_parameter(param, msg="No se puede interpretar el parámetro {}."):
    print(msg.format(param))
    print_usage()
    quit()

def parse_ids(s):
    if '-' in s[1:-1]:
        ids = s.split('-')
        if len(ids) == 2 and ids[0].isdigit() and ids[1].isdigit():
            l = int(ids[0])
            h = int(ids[1])
            return list(range(min(l, h), 1+max(l, h)))
        else:
            error_on_parameter(s)
    else:
        if s.isdigit():
            return [int(s)]
        else:
            error_on_parameter(s)

def main():
    if len(sys.argv) < 2:
        print("Debe especificar al menos un identificador")
        print_usage()
    else:
        ids = frozenset(functools.reduce(lambda x, y: x + y, map(parse_ids, sys.argv[1:])))
        with open("salida.csv", "w") as f:
            f.write(Record.title_row() + '\n')
            for id in ids:
                try:
                    record = read_prospero_record(id)
                    if record['recordtype'].strip().lower() != "clinical":
                        f.write('{},"No es un estudio clínico"\n'.format(id))
                    else:
                        f.write(Record(record).as_row + '\n')
                except:
                    f.write('{},"No se pudo leer el registro"\n'.format(id))
                time.sleep(1)

if __name__ == "__main__":
    main()




