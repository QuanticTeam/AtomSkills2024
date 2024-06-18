import { ReactNode, useContext } from 'react'
import { AuthContext, AuthContextValue } from '../auth'

interface ConditionProps {
  conditions: Array<({ authInfo }: { authInfo: AuthContextValue['authInfo'] }) => any>
  children: ReactNode
  every?: boolean
}

export function Condition({ children, conditions, every = false }: ConditionProps) {
  const { authInfo } = useContext(AuthContext)

  if (conditions[every ? 'every' : 'some'](c => c({ authInfo }))) return children

  return null
}
