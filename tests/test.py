'''
Rode todos os teste utilizando:
    > pytest tests/
Ou rode um test eindividual utilizando:
    > pytest tests.py::TestClass::<method>
'''

from main import simple_calculator

class TestClass:
    def 'should return a correct sum value'(self):

        text = '1+2'
        assert simple_calculator(text) == 3

    def 'should return a correct subtract value'(self):

        text = '3-2'
        assert simple_calculator(text) == 1

    def 'should return a correct operation value'(self):

        text = ' 11+22-33'
        assert simple_calculator(text) == 0
    
    def 'should return a correct operation value | Input with spaces'(self):

        text = '789 +345 - 123'
        assert simple_calculator(text) == 1011

    def 'should raise a error | Insufficient operators | Space between numbers'(self):
        try:
            text = ' 11+22 33'
            simple_calculator(text)
            assert False
        except Exception:
            assert True

    def 'should raise a error | Wrong sequence 1'(self):
        try:
            text = ' 11+-22 33'
            simple_calculator(text)
            assert False
        except Exception:
            assert True
    
    def 'should raise a error | Wrong sequence 2'(self):
        try:
            text = ' 11+- 33'
            simple_calculator(text)
            assert False
        except Exception:
            assert True
        
    def 'should raise a error | Just numbers'(self):
        try:
            text = ' 11 33'
            simple_calculator(text)
            assert False
        except Exception:
            assert True

    def 'should raise a error | Just operators'(self):
        try:
            text = ' +-'
            simple_calculator(text)
            assert False
        except Exception:
            assert True

    def 'should raise a error | Operations end with operator'(self):
        try:
            text = ' 2 +1 -'
            simple_calculator(text)
            assert False
        except Exception:
            assert True