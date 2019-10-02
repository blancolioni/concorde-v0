import React from 'react';
import { BrowserRouter as Router, Route } from 'react-router-dom';

import { PrivateRoute } from '../_components';
import { Concorde } from '../Concorde';
import { LoginPage } from '../authentication/LoginPage';

class App extends React.Component {
    render() {
        return (
                    <Router>
                        <div>
                            <PrivateRoute exact path="/" component={Concorde} />
                            <Route path="/login" component={LoginPage} />
                        </div>
                    </Router>
        );
    }
}

export { App }; 