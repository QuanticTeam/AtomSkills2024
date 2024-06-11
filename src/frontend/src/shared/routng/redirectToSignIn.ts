import { redirect } from 'react-router-dom'
import { ROUTE_PATH_SIGN_IN } from './constants'

export const redirectToSignIn = () => redirect(ROUTE_PATH_SIGN_IN)
