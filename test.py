'''
Rode todos os teste utilizando:
    > pytest test.py
Ou rode um test eindividual utilizando:
    > pytest test.py::TestClass::<method>
'''

from main import simple_calculator

class TestClass:
    def test1(self):
        '''
            Should return a correct sum value
        '''
        text = ' 1+2'
        assert simple_calculator(text) == 3

    def test2(self):
        '''
            Should return a correct subtract value
        '''
        text = '3-2'
        assert simple_calculator(text) == 1

    def test3(self):
        '''
            Should return a correct operation value
        '''
        text = ' 11+22-33'
        assert simple_calculator(text) == 0
    
    def test4(self):
        '''
            Should return a correct operation value | Input with spaces
        '''
        text = '789 +345 - 123'
        assert simple_calculator(text) == 1011

    def test5(self):
        '''
            Should raise a error | Insufficient operators | Space between numbers
        '''
        try:
            text = ' 11+22 33'
            simple_calculator(text)
            assert False
        except Exception:
            assert True

    def test6(self):
        '''
            Should raise a error | Wrong sequence 1
        '''
        try:
            text = ' 11+-22 33'
            simple_calculator(text)
            assert False
        except Exception:
            assert True
    
    def test7(self):
        '''
            Should raise a error | Wrong sequence 2
        '''
        try:
            text = ' 11+- 33'
            simple_calculator(text)
            assert False
        except Exception:
            assert True
        
    def test8(self):
        '''
            Should raise a error | Just numbers
        '''
        try:
            text = ' 11 33'
            simple_calculator(text)
            assert False
        except Exception:
            assert True

    def test9(self):
        '''
            Should raise a error | Numbers without operator
        '''
        try:
            text = ' 11 33 +1'
            assert simple_calculator(text) != 1134
            assert False
        except Exception:
            assert True

    def test10(self):
        '''
            Should raise a error | Just operators'
        '''
        try:
            text = ' +-'
            simple_calculator(text)
            assert False
        except Exception:
            assert True

    def test11(self):
        '''
            Should raise a error | Operations end with operator
        '''
        try:
            text = ' 2 +1 -'
            simple_calculator(text)
            assert False
        except Exception:
            assert True

    def test12(self):
        '''
            Should raise a error | Begin with operator
        '''
        try:
            text = ' +2 +1 '
            simple_calculator(text)
            assert False
        except Exception:
            assert True